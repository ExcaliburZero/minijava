package minijava.parser

import minijava.grammar._
import org.antlr.v4.runtime.tree.TerminalNode

class MiniJavaVisitorImpl extends MiniJavaBaseVisitor[ASTNode] {
  override def visitGoal(ctx: MiniJavaParser.GoalContext): ASTNode = {
    val mainClass = ctx.mainClass().accept(this)
      .asInstanceOf[MainClass]

    val classDeclarations: List[ClassDeclaration] = ctx.classDeclaration().toArray
        .map(
          _.asInstanceOf[MiniJavaParser.ClassDeclarationContext]
            .accept(this)
            .asInstanceOf[ClassDeclaration]
        ).toList

    Goal(mainClass, classDeclarations)
  }

  override def visitMainClass(ctx: MiniJavaParser.MainClassContext): ASTNode = {
    val className = Identifier(ctx.IDENTIFIER(0).getSymbol.getText)
    val isIo = ctx.io != null
    val parameter = Identifier(ctx.IDENTIFIER(1).getSymbol.getText)
    val statement = ctx.statement().accept(this).asInstanceOf[Statement]

    MainClass(className, isIo, parameter, statement)
  }

  override def visitClassDeclaration(ctx: MiniJavaParser.ClassDeclarationContext): ASTNode = {
    val className = Identifier(ctx.IDENTIFIER(0).getSymbol.getText)

    val parentClass = if (ctx.IDENTIFIER().size() > 1) {
      Some(Identifier(ctx.IDENTIFIER(1).getSymbol.getText))
    } else {
      None
    }

    val variableDeclarations = ctx.varDeclaration().toArray
      .map(_.asInstanceOf[MiniJavaParser.VarDeclarationContext]
        .accept(this)
        .asInstanceOf[VariableDeclaration]
      ).toList
    val methodDeclarations = ctx.methodDeclaration().toArray
      .map(_.asInstanceOf[MiniJavaParser.MethodDeclarationContext]
        .accept(this)
        .asInstanceOf[MethodDeclaration]
      ).toList

    ClassDeclaration(className, parentClass, variableDeclarations, methodDeclarations)
  }

  override def visitVarDeclaration(ctx: MiniJavaParser.VarDeclarationContext): ASTNode = {
    val varType = ctx.`type`().accept(this).asInstanceOf[Type]
    val name = Identifier(ctx.IDENTIFIER().getSymbol.getText)

    VariableDeclaration(varType, name)
  }

  override def visitMethodDeclaration(ctx: MiniJavaParser.MethodDeclarationContext): ASTNode = {
    val isIO = ctx.io != null
    val varType = ctx.`type`(0).accept(this).asInstanceOf[Type]
    val name = Identifier(ctx.IDENTIFIER(0).getSymbol.getText)

    val paramTypes = ctx.`type`().toArray.drop(1)
      .map(_.asInstanceOf[MiniJavaParser.TypeContext].accept(this).asInstanceOf[Type])
    val paramNames = ctx.IDENTIFIER().toArray.drop(1)
      .map(i => Identifier(i.asInstanceOf[TerminalNode].getSymbol.getText))

    val parameters = paramTypes.zip(paramNames).toList
    val variableDeclarations = ctx.varDeclaration().toArray
      .map(
        _.asInstanceOf[MiniJavaParser.VarDeclarationContext].accept(this).asInstanceOf[VariableDeclaration]
      ).toList
    val statements = ctx.statement().toArray
      .map(
        _.asInstanceOf[MiniJavaParser.StatementContext].accept(this).asInstanceOf[Statement]
      ).toList
    val returnExpression = ctx.expression().accept(this).asInstanceOf[Expression]

    MethodDeclaration(isIO, varType, name, parameters, variableDeclarations, statements, returnExpression)
  }

  override def visitIntArrayType(ctx: MiniJavaParser.IntArrayTypeContext): ASTNode = {
    IntArrayType
  }

  override def visitBooleanType(ctx: MiniJavaParser.BooleanTypeContext): ASTNode = {
    BooleanType
  }

  override def visitIntType(ctx: MiniJavaParser.IntTypeContext): ASTNode = {
    IntType
  }

  override def visitIdentifierType(ctx: MiniJavaParser.IdentifierTypeContext): ASTNode = {
    val name = Identifier(ctx.IDENTIFIER().getSymbol.getText)

    IdentifierType(name)
  }

  override def visitStatementBlock(ctx: MiniJavaParser.StatementBlockContext): ASTNode = {
    val statements = ctx.statement().toArray
      .map(
        _.asInstanceOf[MiniJavaParser.StatementContext].accept(this).asInstanceOf[Statement]
      ).toList

    StatementBlock(statements)
  }

  override def visitIfStatement(ctx: MiniJavaParser.IfStatementContext): ASTNode = {
    val condition = ctx.expression().accept(this).asInstanceOf[Expression]
    val thenClause = ctx.statement(0).accept(this).asInstanceOf[Statement]
    val elseClause = ctx.statement(1).accept(this).asInstanceOf[Statement]

    IfStatement(condition, thenClause, elseClause)
  }

  override def visitWhileStatement(ctx: MiniJavaParser.WhileStatementContext): ASTNode = {
    val condition = ctx.expression().accept(this).asInstanceOf[Expression]
    val statement = ctx.statement().accept(this).asInstanceOf[Statement]

    WhileStatement(condition, statement)
  }

  override def visitPrintStatement(ctx: MiniJavaParser.PrintStatementContext): ASTNode = {
    val expression = ctx.expression().accept(this).asInstanceOf[Expression]

    PrintStatement(expression)
  }

  override def visitAssignmentStatement(ctx: MiniJavaParser.AssignmentStatementContext): ASTNode = {
    val name = Identifier(ctx.IDENTIFIER().getSymbol.getText)
    val expression = ctx.expression().accept(this).asInstanceOf[Expression]

    AssignmentStatement(name, expression)
  }

  override def visitArrayAssignmentStatement(ctx: MiniJavaParser.ArrayAssignmentStatementContext): ASTNode = {
    val name = Identifier(ctx.IDENTIFIER().getSymbol.getText)
    val indexExpression = ctx.expression(0).accept(this).asInstanceOf[Expression]
    val valueExpression = ctx.expression(1).accept(this).asInstanceOf[Expression]

    ArrayAssignmentStatement(name, indexExpression, valueExpression)
  }

  override def visitBinaryOperationExpression(ctx: MiniJavaParser.BinaryOperationExpressionContext): ASTNode = {
    val firstExpression = ctx.expression(0).accept(this).asInstanceOf[Expression]

    val operator = ctx.BINARY_OPERATOR().getSymbol.getText match {
      case "&&" => And
      case "<" => LessThan
      case "+" => Plus
      case "-" => Minus
      case "*" => Times
    }

    val secondExpression = ctx.expression(1).accept(this).asInstanceOf[Expression]

    BinaryOperationExpression(firstExpression, operator, secondExpression)
  }

  override def visitArrayAccessExpression(ctx: MiniJavaParser.ArrayAccessExpressionContext): ASTNode = {
    val arrayExpression = ctx.expression(0).accept(this).asInstanceOf[Expression]
    val indexExpression = ctx.expression(1).accept(this).asInstanceOf[Expression]

    ArrayAccessExpression(arrayExpression, indexExpression)
  }

  override def visitArrayLengthExpression(ctx: MiniJavaParser.ArrayLengthExpressionContext): ASTNode = {
    val arrayExpression = ctx.expression().accept(this).asInstanceOf[Expression]

    ArrayLengthExpression(arrayExpression)
  }

  override def visitMethodCallExpression(ctx: MiniJavaParser.MethodCallExpressionContext): ASTNode = {
    val objectExpression = ctx.objectExpression.accept(this).asInstanceOf[Expression]
    val methodName = Identifier(ctx.IDENTIFIER().getSymbol.getText)
    val parameters = ctx.expression().toArray().drop(1)
      .map(_.asInstanceOf[MiniJavaParser.ExpressionContext].accept(this).asInstanceOf[Expression])
      .toList

    MethodCallExpression(objectExpression, methodName, parameters)
  }

  override def visitIdentifierExpression(ctx: MiniJavaParser.IdentifierExpressionContext): ASTNode = {
    IdentifierExpression(Identifier(ctx.IDENTIFIER().getSymbol.getText))
  }

  override def visitIntegerLiteral(ctx: MiniJavaParser.IntegerLiteralContext): ASTNode = {
    IntegerLiteral(Integer.parseInt(ctx.INTEGER_LITERAL().getSymbol.getText))
  }

  override def visitTrue(ctx: MiniJavaParser.TrueContext): ASTNode = {
    TrueLiteral
  }

  override def visitFalse(ctx: MiniJavaParser.FalseContext): ASTNode = {
    FalseLiteral
  }

  override def visitThis(ctx: MiniJavaParser.ThisContext): ASTNode = {
    ThisLiteral
  }

  override def visitNewIntArrayExpression(ctx: MiniJavaParser.NewIntArrayExpressionContext): ASTNode = {
    val lengthExpression = ctx.expression().accept(this).asInstanceOf[Expression]

    NewIntArrayExpression(lengthExpression)
  }

  override def visitNewObjectExpression(ctx: MiniJavaParser.NewObjectExpressionContext): ASTNode = {
    NewObjectExpression(Identifier(ctx.IDENTIFIER().getSymbol.getText))
  }

  override def visitNegationExpression(ctx: MiniJavaParser.NegationExpressionContext): ASTNode = {
    val expression = ctx.expression().accept(this).asInstanceOf[Expression]

    NegatedExpression(expression)
  }

  override def visitParenedExpression(ctx: MiniJavaParser.ParenedExpressionContext): ASTNode = {
    val expression = ctx.expression().accept(this).asInstanceOf[Expression]

    ParenedExpression(expression)
  }
}
