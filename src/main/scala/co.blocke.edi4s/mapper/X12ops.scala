package co.blocke.edi4s
package mapper

import model.*

import scala.annotation.tailrec

object X12ops:

  case class HLNode(
                     hlId: String, // HL01
                     hlParentId: Option[String], // HL02
                     hlType: String, // HL03 (e.g. S, O, T, P, I)
                     segmentGroup: List[SegmentX12Token],
                     children: List[HLNode] = Nil
                   )

  case class HLParseResult(
                            roots: List[HLNode],
                            remaining: List[SegmentX12Token] // non-HL segments after the HL tree ends
                          )

  def extractHLRange(
                      segments: List[SegmentX12Token],
                      loopSpec: RefinedLoopSpec
                    ): (List[SegmentX12Token], List[SegmentX12Token]) =

    @tailrec
    def walk(
              remaining: List[SegmentX12Token],
              acc: List[SegmentX12Token]
            ): (List[SegmentX12Token], List[SegmentX12Token]) =
      remaining match
        case Nil => (acc.reverse, Nil)

        case seg :: tail if seg.name == "HL" || !Set("SE", "GE", "REF", "GZ").contains(seg.name) =>
          // Accept all HL segments and other body segments until we hit a terminal marker
          walk(tail, seg :: acc)

        case seg :: tail =>
          println(s"END (terminal or control segment): ${seg.name}")
          (acc.reverse, remaining)

    walk(segments, Nil)


  def extractHLTree(body: List[SegmentX12Token]): HLParseResult =
    HLParseResult(buildHLTree(body), Nil)


  // Builds a tree of HLNodes from the flat list of segments in ST body
  private def buildHLTree(segments: List[SegmentX12Token]): List[HLNode] =
    // Step 1: Extract all HL blocks
    val hlGroups = segments.foldLeft(List.empty[List[SegmentX12Token]]) {
      case (accum, seg) if seg.name == "HL" =>
        List(seg) :: accum  // Start a new group
      case (head :: tail, seg) =>
        (seg :: head) :: tail  // Append to current group
      case (Nil, seg) =>
        List(List(seg)) // First group
    }.map(_.reverse).reverse  // Restore original order

    // Step 2: Build HLNode map
    val nodeMap = hlGroups.flatMap {
      case hlSeg :: body =>
        val hl01 = hlSeg.fields.lift(0).collect { case s: SimpleX12Token => s.value }.getOrElse("?")
        val hl02 = hlSeg.fields.lift(1).collect { case s: SimpleX12Token => s.value }
        val hl03 = hlSeg.fields.lift(2).collect { case s: SimpleX12Token => s.value }.getOrElse("?")
        Some(hl01 -> HLNode(hl01, hl02, hl03, hlSeg :: body))
      case _ => None
    }.toMap

    // Step 3: Link children recursively
    def linkChildren(parentId: String): List[HLNode] =
      nodeMap.values
        .filter(_.hlParentId.contains(parentId))
        .toList
        .sortBy(_.hlId.toInt)
        .map(child => child.copy(children = linkChildren(child.hlId)))

    // Step 4: Build the tree
    nodeMap.values
      .filter(_.hlParentId.isEmpty)
      .toList
      .sortBy(_.hlId.toInt)
      .map(root => root.copy(children = linkChildren(root.hlId)))


  // Flatten all branches down to any HL level in `flattenLevels`
  def flattenHLTree(
                     roots: List[HLNode],
                     flattenLevels: Set[String],
                     canonicalSpec: RefinedLoopSpec
                   ): List[SegmentX12Token] =
    val canonicalOrder = linearizeCanonicalOrder(canonicalSpec).zipWithIndex.toMap

    def recurse(
                 node: HLNode,
                 inherited: List[SegmentX12Token],
                 effectiveParentHLId: Option[String]
               ): List[SegmentX12Token] =

      val isFlattened = flattenLevels.contains(node.hlType)
      val rest = node.segmentGroup.tail
      val newInherited =
        if isFlattened then overrideSegments(inherited, rest)
        else inherited

      val childOutput = node.children.sortBy(_.hlId.toInt).flatMap(child =>
        recurse(child, newInherited, if isFlattened then effectiveParentHLId else Some(node.hlId))
      )

      if isFlattened then
        childOutput
      else
        val hlSegment = node.segmentGroup.head
        val body = overrideSegments(inherited, rest)

        val rewrittenGroup = (hlSegment +: body).map {
          case s@SegmentX12Token("HL", f0 :: _ :: f2 :: Nil) if effectiveParentHLId.isDefined =>
            s.copy(fields = List(f0, SimpleX12Token("HL02", effectiveParentHLId.get), f2))
          case other => other
        }

        val sortedGroup = interleaveSegments(Nil, rewrittenGroup, canonicalOrder)
        sortedGroup ++ childOutput

    roots.sortBy(_.hlId.toInt).flatMap(r => recurse(r, Nil, None))


  private def overrideSegments(
                        parent: List[SegmentX12Token],
                        child: List[SegmentX12Token]
                      ): List[SegmentX12Token] =
    val childMap = child.map(_.name).toSet
    val base = parent.filterNot(s => childMap.contains(s.name))
    base ++ child

  private def interleaveSegments(
                          parent: List[SegmentX12Token],
                          child: List[SegmentX12Token],
                          canonicalOrder: Map[String, Int]
                        ): List[SegmentX12Token] =
    val combined = overrideSegments(parent, child)
    combined.sortBy(seg => canonicalOrder.getOrElse(seg.name, Int.MaxValue))


  private def linearizeCanonicalOrder(spec: RefinedLoopSpec): List[String] =
    def flatten(loop: RefinedLoopSpec): List[String] =
      // Start with the loop’s own name
      val loopSelf = List(loop.canonicalName)

      val bodyNames = loop.body.flatMap {
        case seg: RefinedSegmentSpec =>
          List(seg.canonicalName)
        case nested: RefinedLoopSpec =>
          flatten(nested)
      }

      loopSelf ++ bodyNames

    flatten(spec)


  def flattenHLSpec(
                     flattenLevels: Set[String],
                     partnerSpec: RefinedLoopSpec,
                     canonicalSpec: RefinedLoopSpec
                   ): RefinedLoopSpec = {

    def linearizeCanonicalOrder(spec: RefinedLoopSpec): List[String] =
      def recurse(spec: RefinedLoopSpec): List[String] =
        val base = spec.body.flatMap {
          case s: RefinedSegmentSpec => List(s.name)
          case l: RefinedLoopSpec => l.name +: recurse(l) // << fix: include the loop name
        }
        base ++ spec.nested.toList.flatMap(recurse)

      recurse(spec)

    def interleaveLoopBodies(
                              parentBody: List[RefinedSingleOrLoopSegmentSpec],
                              childBody: List[RefinedSingleOrLoopSegmentSpec],
                              canonicalOrder: List[String]
                            ): List[RefinedSingleOrLoopSegmentSpec] = {
      val parentSegs = parentBody.collect { case s: RefinedSegmentSpec => s.name -> s }.toMap
      val childSegs = childBody.collect { case s: RefinedSegmentSpec => s.name -> s }.toMap
      val parentLoops = parentBody.collect { case l: RefinedLoopSpec => l.name -> l }.toMap
      val childLoops = childBody.collect { case l: RefinedLoopSpec => l.name -> l }.toMap

      val mergedFlatSegs = canonicalOrder.flatMap { name =>
        childSegs.get(name).orElse(parentSegs.get(name))
      }

      val mergedLoops = canonicalOrder.flatMap { name =>
        (childLoops.get(name), parentLoops.get(name)) match {
          case (Some(child), Some(parent)) =>
            Some(child.copy(body = interleaveLoopBodies(parent.body, child.body, canonicalOrder)))
          case (Some(child), None) => Some(child)
          case (None, Some(parent)) => Some(parent)
          case _ => None
        }
      }

      mergedFlatSegs ++ mergedLoops
    }

    def recurse(partner: RefinedLoopSpec, canon: RefinedLoopSpec): RefinedLoopSpec = {
      val nestedFlattened = partner.nested.map(n => recurse(n, canon))
      val shouldFlatten = flattenLevels.contains(partner.description)

      if (shouldFlatten && nestedFlattened.isDefined) {
        val flattened = nestedFlattened.get
        val canonicalOrder = linearizeCanonicalOrder(canonicalSpec)
        val newBody = interleaveLoopBodies(partner.body, flattened.body, canonicalOrder)
        flattened.copy(body = newBody)
      } else {
        partner.copy(nested = nestedFlattened)
      }
    }

    recurse(partnerSpec, canonicalSpec)
  }


  def show(spec: RefinedDocumentSpec): Unit =
    println(s"Document: ${spec.name} (v${spec.version}, partner: ${spec.partner})")
    spec.segments.foreach {
      case loop: RefinedLoopSpec =>
        showLoop(loop, indent = 1)
      case seg: RefinedSegmentSpec =>
        showSegment(seg, indent = 1)
    }

  def showLoop(loop: RefinedLoopSpec, indent: Int): Unit =
    if loop.isRealLoop then
      println(s"${"  " * indent}Segment (loop): ${loop.name}[${loop.description}]")
    else
      println(s"${"  " * indent}Segment: ${loop.name}")
//    loop.fields.foreach(f => println(s"${"  " * (indent + 1)}Field: ${f.name}"))
    loop.body.foreach {
      case seg: RefinedSegmentSpec =>
        showSegment(seg, indent + 1)
      case nestedLoop: RefinedLoopSpec =>
        showLoop(nestedLoop, indent + 1)
    }
    if loop.nested.nonEmpty then print(s"${" "*(indent+1)}(nested) ")
    loop.nested.foreach(n => showLoop(n, indent + 1))

  def showSegment(seg: RefinedSegmentSpec, indent: Int): Unit =
    println(s"${"  " * indent}Segment: ${seg.name}")
//    seg.fields.foreach(f => println(s"${"  " * (indent + 1)}Field: ${f.name}"))