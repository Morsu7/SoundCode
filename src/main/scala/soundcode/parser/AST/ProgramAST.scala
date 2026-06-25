package soundcode.parser.AST

sealed trait Block

// Each block contains a pattern
case class SoundBlock(pattern: Pattern[Sample]) extends Block
case class NoteBlock(pattern: Pattern[Note], attachment: Option[SoundBlock]) extends Block

// A Pattern is a series of sequences played in parallel, each sequence lasts exactly for a cycle
case class Pattern[A <: Atom](elems: List[Sequence[A]])

// A sequence is a series of elements played in sequence, each element lasts for a fraction of the cycle
case class Sequence[A <: Atom](elems: List[Element[A]])

// An element is either an atom (note/sample) or a sub-pattern 
sealed trait Element[A <: Atom]
case class AtomElement[A <: Atom](atom: A) extends Element[A]
case class SubPatternElement[A <: Atom](pattern: Pattern[A]) extends Element[A]
case class AlternationElement[A <: Atom](pattern: Pattern[A]) extends Element[A] // A pattern contained in <> brackets, played in alternation (one element per cycle, round-robin)

sealed trait Atom
case class Note(value: String) extends Atom
case class Sample(value: String) extends Atom

case class ProgramAST(blocks: Seq[Block]) {
    override def toString: String = {
        val sb = new StringBuilder("ProgramAST\n")
        blocks.zipWithIndex.foreach { case (block, idx) =>
            sb.append(formatBlock(block, "", idx == blocks.size - 1))
            if (idx < blocks.size - 1) sb.append("\n")
        }
        sb.toString()
    }

    // Versione ASCII sicura al 100% per qualsiasi terminale / OS encoding
    private def formatBlock(block: Block, indent: String, isLast: Boolean): String = {
        val marker = if (isLast) "`-- " else "|-- "
        val nextIndent = indent + (if (isLast) "    " else "|   ")
        block match {
            case SoundBlock(pat) =>
                s"$indent${marker}SoundBlock\n${formatPattern(pat, nextIndent, isLast = true)}"
            case NoteBlock(pat, attach) =>
                val base = s"$indent${marker}NoteBlock\n${formatPattern(pat, nextIndent, attach.isEmpty)}"
                attach.map(sub => s"$base\n$indent|   `-- .attachment\n${formatBlock(sub, nextIndent + "    ", isLast = true)}").getOrElse(base)
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
        val nextIndent = indent + (if (isLast) "    " else "│   ")
        el match {
            case AtomElement(atom) => s"$indent${marker}Atom: ${atom match { case Note(v) => s"Note($v)"; case Sample(v) => s"Sample($v)" }}"
            case SubPatternElement(p) => s"$indent${marker}SubPatternElement [Square brackets contains]\n${formatPattern(p, nextIndent, isLast = true)}"
            case AlternationElement(p) => s"$indent${marker}AlternationElement [Angular brackets contains]\n${formatPattern(p, nextIndent, isLast = true)}"
        }
    }
}