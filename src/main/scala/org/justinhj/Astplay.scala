package org.justinhj

// Playing around with Visitor pattern for AST manipulation

object Astplay {

    // sealed trait ProgramEntity
    // case class Program(expressions: List[Expression]) extends ProgramEntity

    trait Expression //extends ProgramEntity
    case class OperatorExpression(left: Expression, operation: Operator, right: Expression) extends Expression
    case class Value(n: Int) extends Expression

    sealed trait Operator
    case object Plus extends Operator
    case object Multiply extends Operator

    val e1 = OperatorExpression(Value(3), Multiply, Value(4))
    val e2 = OperatorExpression(e1, Plus, Value(13))
    //val s1 = Program(List(e2))


    /*
    * http://www.lihaoyi.com/post/ZeroOverheadTreeProcessingwiththeVisitorPattern.html
    * The Visitor Pattern is one of the most mis-understood of the 
    * classic design patterns. While it has a reputation as a slightly 
    * roundabout technique for doing simple processing on simple trees, 
    * it is actually an advanced tool for a specific use case: flexible, 
    * streaming, zero-overhead processing of complex data structures
    */

    // Visitor pattern. T is the return type based on whatever the caller wants to do

    abstract class Visitor[T]{
      def visitValue(value: Int): T
      def visitOperatorExpression() : OperatorExpressionVisitor[T] 
    }

    abstract class OperatorExpressionVisitor[T] {
      //def visitValue(value: T): Unit
      def visitValue(): Visitor[T]

      def visitOp(left: T, op: Operator, right: T) : T
    }

    // Visitor plus this dispatcher is what you need to implement the visitor pattern
    // Note that the dispatch is writtern for a particular AST in this case 
    // my expression language above
    def dispatch[T](input: Expression, visitor: Visitor[T]): T = {
      input match{
        case Value(value) => 
          visitor.visitValue(value)
        case OperatorExpression(left, op, right) =>
          val operatorVisitor = visitor.visitOperatorExpression()
          val l = dispatch(left, operatorVisitor.visitValue())
          val r = dispatch(right, operatorVisitor.visitValue())
          operatorVisitor.visitOp(l,op,r)
      }
    }

    // Convert expression to string

    class StringifyVisitor extends Visitor[String]{
      def visitValue(value: Int) = value.toString
      def visitOperatorExpression() = new StringifyOperatorExpressionVisitor
    }

    class StringifyOperatorExpressionVisitor extends OperatorExpressionVisitor[String] {
     
      def visitValue(): Visitor[String] = new StringifyVisitor

      def visitOp(left: String, op: Operator, right: String) : String = {
        op match {
          case Plus =>
            s"[Plus $left, $right]"
          case Multiply => 
            s"[Multiply $left, $right]"
        }
      }

    }

    // Evaluate expression

    class EvaluationVisitor extends Visitor[Int]{
      def visitValue(value: Int) = value
      def visitOperatorExpression() = new EvaluationOperatorExpressionVisitor
    }

    class EvaluationOperatorExpressionVisitor extends OperatorExpressionVisitor[Int] {
     
      def visitValue(): Visitor[Int] = new EvaluationVisitor

      def visitOp(left: Int, op: Operator, right: Int) : Int = {
        op match {
          case Plus =>
            left + right
          case Multiply => 
            left * right
        }
      }

    }
    // Pattern matching execution

    // We will convert to another AST
    // sealed trait ResultEntity
    // case class ResultEntities(results: List[ResultEntity]) extends ResultEntity
    // case class ResultValue(n: Int) extends ResultEntity

    // def execute(entity: ProgramEntity) : ResultEntity  = entity match {
    //   case Program(expressions) =>
    //     ResultEntities(expressions.map(execute(_)))
    //   case Value(n) => 
    //     ResultValue(n)
    //   case OperatorExpression(l : OperatorExpression,op,r) =>
    //     val left = execute(l)
    //     //execute(OperatorExpression(left, op, r))
    //     ???
    //   case OperatorExpression(Value(l),Multiply,Value(r)) =>
    //     ResultValue(l * r)
    // }

    def main(args : Array[String]) : Unit = {

      println(s"e2 to string: ${dispatch(e2, new StringifyVisitor)}")

      println(s"e2 evaluated: ${dispatch(e2, new EvaluationVisitor)}")
    }
}


