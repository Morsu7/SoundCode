package soundcode.utils.parser

import soundcode.parser.AST._
import soundcode.parser.AST.Transformations._

object ASTPrinter {
  extension (program: ProgramAST) {
    def renderTree: String = {
      val sb = new StringBuilder("ProgramAST\n")
      program.blocks.zipWithIndex.foreach { case (block, idx) =>
        sb.append(formatBlock(block, "", idx == program.blocks.size - 1))
        if (idx < program.blocks.size - 1) sb.append("\n")
      }
      sb.toString()
    }
  }

  private def formatBlock(block: Block, indent: String, isLast: Boolean): String = {
    val marker = if (isLast) "`-- " else "|-- "
    val nextIndent = indent + (if (isLast) "    " else "|   ")
    
    block match {
      case StreamBlock(base, extensions) =>
        val baseStr = s"$indent${marker}StreamBlock\n" + formatBlock(base, nextIndent, extensions.isEmpty)
        if (extensions.isEmpty) baseStr
        else {
          val extStr = extensions.zipWithIndex.map { case (ext, idx) =>
            formatBlock(ext, nextIndent, idx == extensions.size - 1)
          }.mkString("\n")
          s"$baseStr\n$extStr"
        }

      case GenerativeExtensionBlock(genBlock) =>
        s"$indent${marker}GenerativeExtensionBlock (.)\n${formatBlock(genBlock, nextIndent, isLast = true)}"

      case TransformationExtensionBlock(transBlock) =>
        s"$indent${marker}TransformationExtensionBlock (.)\n${formatBlock(transBlock, nextIndent, isLast = true)}"

      case t: TransformationBlock =>
        val (name, pat) = t match {
          case Gain(p)            => ("gain", p)
          case Pan(p)             => ("pan", p)
          case Room(p)            => ("room", p)
          case Delay(p)           => ("delay", p)
          case LowPassFilter(p)   => ("lowPassFilter", p)
          case HighPassFilter(p)  => ("highPassFilter", p)
          case _ => ("unknown", Pattern(List()))
        }
        s"$indent${marker}TransformationBlock: .$name(...)\n${formatPattern(pat, nextIndent, isLast = true)}"

      case SoundBlock(pat) =>
        s"$indent${marker}SoundBlock\n${formatPattern(pat, nextIndent, isLast = true)}"
        
      case NoteBlock(pat) =>
        s"$indent${marker}NoteBlock\n${formatPattern(pat, nextIndent, isLast = true)}"
    }
  }

  private def formatPattern[A <: Atom](pat: Pattern[A], indent: String, isLast: Boolean): String = {
    val marker = if (isLast) "`-- " else "|-- "
    val nextIndent = indent + (if (isLast) "    " else "|   ")
    val children = pat.elems.zipWithIndex.map { case (seq, idx) =>
      val sMarker = if (idx == pat.elems.size - 1) "`-- " else "|-- "
      val sIndent = nextIndent + (if (idx == pat.elems.size - 1) "    " else "|   ")
      s"$nextIndent${sMarker}Sequence (Whitespace separated)\n${seq.elems.zipWithIndex.map { case (el, elIdx) => 
        formatElement(el, sIndent, elIdx == seq.elems.size - 1)
      }.mkString("\n")}"
    }.mkString("\n")
    s"$indent${marker}Pattern (Comma separated)\n$children"
  }

  private def formatElement[A <: Atom](el: Element[A], indent: String, isLast: Boolean): String = {
    val marker = if (isLast) "`-- " else "|-- "
    val nextIndent = indent + (if (isLast) "    " else "|   ")
    el match {
      case AtomElement(atom) => s"$indent${marker}Atom: ${atom match { case Note(v) => s"Note($v)"; case Sample(v) => s"Sample($v)"; case Config(v) => s"Config($v)" }}"
      case SubPatternElement(p) => s"$indent${marker}SubPatternElement [Square brackets contains]\n${formatPattern(p, nextIndent, isLast = true)}"
      case AlternationElement(p) => s"$indent${marker}AlternationElement [Angular brackets contains]\n${formatPattern(p, nextIndent, isLast = true)}"
    }
  }
}