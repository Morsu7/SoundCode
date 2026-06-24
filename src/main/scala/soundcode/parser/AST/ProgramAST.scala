package soundcode.parser.AST

sealed trait Block

case class Sound(value: Expr) extends Block
case class Note(value: Expr, attachment: Option[Sound]) extends Block

sealed trait Expr
case class Ident(value: String) extends Expr

case class ProgramAST(blocks: Seq[Block])