package minijava.parser

import minijava.grammar._

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

  override def visitPrintStatement(ctx: MiniJavaParser.PrintStatementContext): ASTNode = {
    val expression = ctx.expression().accept(this).asInstanceOf[Expression]

    PrintStatement(expression)
  }

  override def visitMethodCallExpression(ctx: MiniJavaParser.MethodCallExpressionContext): ASTNode = {
    val objectExpression = ctx.objectExpression.accept(this).asInstanceOf[Expression]
    val methodName = Identifier(ctx.IDENTIFIER().getSymbol.getText)
    val parameters = ctx.expression().toArray()
      .map(_.asInstanceOf[MiniJavaParser.ExpressionContext].accept(this).asInstanceOf[Expression])
      .toList

    MethodCallExpression(objectExpression, methodName, parameters)
  }

  override def visitIdentifierExpression(ctx: MiniJavaParser.IdentifierExpressionContext): ASTNode = {
    Identifier(ctx.IDENTIFIER().getSymbol.getText)
  }
}
