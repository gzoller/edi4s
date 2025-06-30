package co.blocke.edi4s
package mapper

import model.*

/** 
 * Execution Context (EC) for running mapping rules. It maintains a stack of Frame, including a program counter (PC).
 * Whenever a loop is entered we push a new Frame containing the body of the loop. When the body has been traversed
 * we pop the frame to continue.
 */

case class Frame(level: String, rules: List[SegmentAssignment], var pc: Int = 0)

case class EC(accOut: List[SegmentX12Token] = Nil, frames: List[Frame] = Nil):

  def pushFrame(level: String, rules: List[SegmentAssignment]): EC =
//    println("  EC PUSHED FRAME: \n" + rules.map(r => r.canonicalName + " :: " + r.getClass.getName).mkString("    ", "\n    ", ""))
    this.copy(frames = Frame(level, rules) :: this.frames)

  def popFrame: EC =
    val popped = this.copy(frames = frames.drop(1))
//    println("  << popped next-rule " + popped.peek.map(_.canonicalName) + ":\n" + popped.frames.head.rules.map(r => r.canonicalName + " :: " + r.getClass.getName).mkString("    ", "\n    ", "")) // skip the rule that caused the push we're now popping
    popped

  def next: Option[SegmentAssignment] =
    frames.headOption.flatMap { f =>
      val result = f.rules.lift(f.pc)
      f.pc += 1
      result
    }

  def backspace: EC = this.copy(frames = frames match {
    case h :: t => h.copy(pc = h.pc - 1) +: t
    case Nil => Nil
  })

  def +(out: SegmentX12Token): EC =
    //      println(" --> Mapped "+out.name)
    this.copy(accOut = this.accOut :+ out)

  def peek: Option[SegmentAssignment] = this.frames.headOption.flatMap(f => f.rules.lift(f.pc))

  def showFrames: String =
    "Frames:\n" + {
      if frames == Nil then "(empty)"
      else
        frames.map(f => "   -------------<<\n" + f.rules.map(r => "   " + r.canonicalName + " -> " + r.getClass.getCanonicalName).mkString("\n")).mkString("\n")
    }