package co.blocke.edi4s
package mapper

import model.*

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

    def walk(remaining: List[SegmentX12Token], acc: List[SegmentX12Token]): (List[SegmentX12Token], List[SegmentX12Token]) =
      remaining match
        case Nil => (acc.reverse, Nil)
        case seg :: tail if seg.name == "HL" =>
          // Assume HL means new loop; include and recurse if nested spec exists
          val (nestedTokens, rest) =
            loopSpec.nested.map { nestedSpec =>
              val (nested, rem) = extractHLRange(tail, nestedSpec)
              (seg :: nested, rem)
            }.getOrElse((List(seg), tail))
          walk(rest, nestedTokens.reverse ++ acc)
        case seg :: tail if loopSpec.body.exists(_.name == seg.name) =>
          walk(tail, seg :: acc)
        case _ =>
          (acc.reverse, remaining) // Stop at unexpected segment

    walk(segments, Nil)


  def extractHLTree(body: List[SegmentX12Token]): HLParseResult =
    HLParseResult(buildHLTree(body), Nil)


  // Builds a tree of HLNodes from the flat list of segments in ST body
  def buildHLTree(segments: List[SegmentX12Token]): List[HLNode] =
    val grouped =
      segments match
        case Nil => Nil
        case _ =>
          val buffer = scala.collection.mutable.ListBuffer.empty[(SegmentX12Token, List[SegmentX12Token])]
          var remaining = segments
          while remaining.nonEmpty do
            val (hl, rest) = (remaining.head, remaining.tail)
            val (body, tail) = rest.span(_.name != "HL")
            buffer += ((hl, body))
            remaining = tail
          buffer.toList

    val nodeMap: Map[String, HLNode] = grouped.map { case (hlSeg, body) =>
      val hl01 = hlSeg.fields.lift(0).collect { case s: SimpleX12Token => s.value }.getOrElse("?")
      val hl02 = hlSeg.fields.lift(1).collect { case s: SimpleX12Token => s.value }
      val hl03 = hlSeg.fields.lift(2).collect { case s: SimpleX12Token => s.value }.getOrElse("?")
      hl01 -> HLNode(hl01, hl02, hl03, hlSeg :: body)
    }.toMap

    def linkChildren(parentId: String): List[HLNode] =
      nodeMap.values
        .filter(_.hlParentId.contains(parentId))
        .toList
        .sortBy(_.hlId.toInt)
        .map(child => child.copy(children = linkChildren(child.hlId)))

    nodeMap.values
      .filter(_.hlParentId.isEmpty)
      .toList
      .sortBy(_.hlId.toInt)
      .map(root => root.copy(children = linkChildren(root.hlId)))


  // Flatten all branches down to any HL level in `flattenLevels`
    /*
  def flattenHLTree(
                     roots: List[HLNode],
                     flattenLevels: Set[String],
                     canonicalSpec: RefinedLoopSpec
                   ): List[SegmentX12Token] =

    val canonicalOrder: Map[String, Int] =
      linearizeCanonicalOrder(canonicalSpec).zipWithIndex.toMap

    def recurse(
                 node: HLNode,
                 inherited: List[SegmentX12Token],
                 effectiveParentHLId: Option[String]
               ): List[SegmentX12Token] =

      val isFlattened = flattenLevels.contains(node.hlType)
      val newInherited =
        if isFlattened then overrideSegments(inherited, node.segmentGroup.drop(1))
        else Nil

      val childOutput = node.children.flatMap { child =>
        recurse(child, newInherited, if isFlattened then effectiveParentHLId else Some(node.hlId))
      }

      if isFlattened then
        childOutput
      else
        val hlSegment = node.segmentGroup.head
        val rest = node.segmentGroup.tail
        val body =
          if isFlattened then interleaveSegments(inherited, rest, canonicalSpec)
          else overrideSegments(inherited, rest) // just parent merge; preserve order

        val rewrittenGroup = (hlSegment +: body).map {
          case s@SegmentX12Token("HL", f0 :: _ :: f2 :: Nil)
            if effectiveParentHLId.isDefined =>
            s.copy(fields = List(f0, SimpleX12Token("HL02", effectiveParentHLId.get), f2))
          case other => other
        }

        rewrittenGroup ++ childOutput

    roots.flatMap(r => recurse(r, Nil, None))

  def overrideSegments(
                        parent: List[SegmentX12Token],
                        child: List[SegmentX12Token]
                      ): List[SegmentX12Token] =
    val childMap = child.map(_.name).toSet
    val base = parent.filterNot(s => childMap.contains(s.name))
    base ++ child


  private def interleaveSegments(
                                  segments: List[SegmentX12Token],
                                  canonicalSpec: RefinedLoopSpec
                                ): List[SegmentX12Token] =
    val canonicalOrder: Map[String, Int] =
      linearizeCanonicalOrder(canonicalSpec).zipWithIndex.toMap
    println("Canonical Sort Order:")
    canonicalOrder.toList.sortBy(_._2).foreach { case (name, idx) =>
      println(f"$idx%02d: $name")
    }
    def sortOneRecord(record: List[SegmentX12Token]): List[SegmentX12Token] =
      record match
        case hl :: rest if hl.name == "HL" =>
          val sorted = rest.sortBy(seg => canonicalOrder.getOrElse(seg.name, Int.MaxValue))
          hl +: sorted
        case other => other

    val grouped: List[List[SegmentX12Token]] = {
      val buf = collection.mutable.ListBuffer.empty[List[SegmentX12Token]]
      var current = collection.mutable.ListBuffer.empty[SegmentX12Token]

      for seg <- segments do
        if seg.name == "HL" then
          if current.nonEmpty then buf += current.toList
          current = collection.mutable.ListBuffer(seg)
        else
          current += seg

      if current.nonEmpty then buf += current.toList
      buf.toList
    }

    grouped.flatMap(sortOneRecord)
    */
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
      val newInherited = if isFlattened then overrideSegments(inherited, rest) else Nil

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


  def overrideSegments(
                        parent: List[SegmentX12Token],
                        child: List[SegmentX12Token]
                      ): List[SegmentX12Token] =
    val childMap = child.map(_.name).toSet
    val base = parent.filterNot(s => childMap.contains(s.name))
    base ++ child

  def interleaveSegments(
                          parent: List[SegmentX12Token],
                          child: List[SegmentX12Token],
                          canonicalOrder: Map[String, Int]
                        ): List[SegmentX12Token] =
    val combined = overrideSegments(parent, child)
    combined.sortBy(seg => canonicalOrder.getOrElse(seg.name, Int.MaxValue))


  def linearizeCanonicalOrder(spec: RefinedLoopSpec): List[String] =
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