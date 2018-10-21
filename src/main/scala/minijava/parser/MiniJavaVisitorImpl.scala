package minijava.parser

import minijava.grammar._
import org.antlr.v4.runtime.tree.TerminalNode
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

class MiniJavaVisitorImpl extends MiniJavaBaseVisitor[Option[ASTNode]] {
  override def visitGoal(ctx: MiniJavaParser.GoalContext): Option[ASTNode] = {
    for (
      mainClRaw <- Option(ctx.mainClass());
      mainCl <- mainClRaw.accept(this).asInstanceOf[Option[MainClass]];
      classDecsRaw <- Option(ctx.classDeclaration());
      classDecs <- classDecsRaw.toArray
        .map(
          _.asInstanceOf[MiniJavaParser.ClassDeclarationContext]
            .accept(this)
            .asInstanceOf[Option[RegularClass]]
        ).toList.sequence
    ) yield Goal(mainCl, classDecs)
  }

  override def visitMainClass(ctx: MiniJavaParser.MainClassContext): Option[ASTNode] = {
    for (
      ctx <- Option(ctx);
      classNameRaw <- Option(ctx.IDENTIFIER(0));
      className = Identifier(classNameRaw.getSymbol.getText);
      isIo = ctx.io != null;
      paramRaw <- Option(ctx.IDENTIFIER(1));
      param = Identifier(paramRaw.getSymbol.getText);
      statmtRaw <- Option(ctx.statement());
      statmt <- Option(statmtRaw.accept(this)).flatMap(_.asInstanceOf[Option[Statement]]);
      lineNumber <- Option(ctx.start.getLine)
    ) yield MainClass(className, isIo, param, statmt, lineNumber)
  }

  override def visitClassDeclaration(ctx: MiniJavaParser.ClassDeclarationContext): Option[ASTNode] = {
    for (
      classNameRaw <- Option(ctx.IDENTIFIER(0));
      className = Identifier(classNameRaw.getSymbol.getText);
      allIdents <- Option(ctx.IDENTIFIER());
      parentClass <- if (allIdents.size() > 1) {
        for (
          pClassRaw <- Option(ctx.IDENTIFIER(1));
          pClass = Some(Identifier(pClassRaw.getSymbol.getText))
        ) yield pClass
      } else {
        Some(None)
      };
      varDecsRaw <- Option(ctx.varDeclaration());
      varDecs <- varDecsRaw.toArray
        .map(_.asInstanceOf[MiniJavaParser.VarDeclarationContext]
          .accept(this)
          .asInstanceOf[Option[VariableDeclaration]]
        ).toList.sequence;
      methodDecsRaw <- Option(ctx.methodDeclaration());
      methodDecs <- methodDecsRaw.toArray
        .map(_.asInstanceOf[MiniJavaParser.MethodDeclarationContext]
          .accept(this)
          .asInstanceOf[Option[MethodDeclaration]]
        ).toList.sequence;
      lineNumber <- Option(ctx.start.getLine)
    ) yield RegularClass(className, parentClass, varDecs, methodDecs, lineNumber)
  }

  override def visitVarDeclaration(ctx: MiniJavaParser.VarDeclarationContext): Option[ASTNode] = {
    for (
      varTypeRaw <- Option(ctx.`type`());
      varType <- varTypeRaw.accept(this).asInstanceOf[Option[Type]];
      idRaw <- Option(ctx.IDENTIFIER());
      id = Identifier(idRaw.getSymbol.getText);
      lineNumber <- Option(ctx.start.getLine)
    ) yield VariableDeclaration(varType, id, lineNumber)
  }

  override def visitMethodDeclaration(ctx: MiniJavaParser.MethodDeclarationContext): Option[ASTNode] = {
    for (
      varTypeRaw <- Option(ctx.`type`(0));
      varType <- varTypeRaw.accept(this).asInstanceOf[Option[Type]];
      isIo = ctx.io != null;
      nameRaw <- Option(ctx.IDENTIFIER(0));
      name = Identifier(nameRaw.getSymbol.getText);
      paramTypesRaw <- Option(ctx.`type`());
      paramTypes <- paramTypesRaw.toArray.drop(1)
        .map(
          _.asInstanceOf[MiniJavaParser.TypeContext].accept(this).asInstanceOf[Option[Type]]
        ).toList.sequence;
      paramNamesRaw <- Option(ctx.IDENTIFIER());
      paramNames = paramNamesRaw.toArray.drop(1)
        .map(i => Identifier(i.asInstanceOf[TerminalNode].getSymbol.getText));
      parameters = paramTypes.zip(paramNames);
      varDecsRaw <- Option(ctx.varDeclaration());
      varDecs <- varDecsRaw.toArray
        .map(
          _.asInstanceOf[MiniJavaParser.VarDeclarationContext].accept(this).asInstanceOf[Option[VariableDeclaration]]
        ).toList.sequence;
      statmtsRaw <- Option(ctx.statement());
      statmts <- statmtsRaw.toArray
        .map(
          _.asInstanceOf[MiniJavaParser.StatementContext].accept(this).asInstanceOf[Option[Statement]]
        ).toList.sequence;
      retExpRaw <- Option(ctx.expression());
      retExp <- retExpRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield MethodDeclaration(isIo, varType, name, parameters, varDecs, statmts, retExp, lineNumber)
  }

  override def visitIntArrayType(ctx: MiniJavaParser.IntArrayTypeContext): Option[ASTNode] = {
    Some(IntArrayType)
  }

  override def visitBooleanType(ctx: MiniJavaParser.BooleanTypeContext): Option[ASTNode] = {
    Some(BooleanType)
  }

  override def visitIntType(ctx: MiniJavaParser.IntTypeContext): Option[ASTNode] = {
    Some(IntType)
  }

  override def visitIdentifierType(ctx: MiniJavaParser.IdentifierTypeContext): Option[ASTNode] = {
    for (
      idRaw <- Option(ctx.IDENTIFIER());
      id = Identifier(idRaw.getSymbol.getText)
    ) yield IdentifierType(id)
  }

  override def visitStatementBlock(ctx: MiniJavaParser.StatementBlockContext): Option[ASTNode] = {
    for (
      statmtsRaw <- Option(ctx.statement());
      statmts <- statmtsRaw.toArray
        .map(
          _.asInstanceOf[MiniJavaParser.StatementContext].accept(this).asInstanceOf[Option[Statement]]
        ).toList.sequence
    ) yield StatementBlock(statmts)
  }

  override def visitIfStatement(ctx: MiniJavaParser.IfStatementContext): Option[ASTNode] = {
    for (
      condRaw <- Option(ctx.expression());
      cond <- condRaw.accept(this).asInstanceOf[Option[Expression]];
      thenClRaw <- Option(ctx.statement(0));
      thenCl <- thenClRaw.accept(this).asInstanceOf[Option[Statement]];
      elseClRaw <- Option(ctx.statement(1));
      elseCl <- elseClRaw.accept(this).asInstanceOf[Option[Statement]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield IfStatement(cond, thenCl, elseCl, lineNumber)
  }

  override def visitWhileStatement(ctx: MiniJavaParser.WhileStatementContext): Option[ASTNode] = {
    for (
      condRaw <- Option(ctx.expression());
      cond <- condRaw.accept(this).asInstanceOf[Option[Expression]];
      statmtRaw <- Option(ctx.statement());
      statmt <- statmtRaw.accept(this).asInstanceOf[Option[Statement]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield WhileStatement(cond, statmt, lineNumber)
  }

  override def visitPrintStatement(ctx: MiniJavaParser.PrintStatementContext): Option[ASTNode] = {
    for (
      expRaw <- Option(ctx.expression());
      exp <- expRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield PrintStatement(exp, lineNumber)
  }

  override def visitAssignmentStatement(ctx: MiniJavaParser.AssignmentStatementContext): Option[ASTNode] = {
    for (
      idRaw <- Option(ctx.IDENTIFIER());
      id = Identifier(idRaw.getSymbol.getText);
      expRaw <- Option(ctx.expression());
      exp <- expRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield AssignmentStatement(id, exp, lineNumber)
  }

  override def visitArrayAssignmentStatement(ctx: MiniJavaParser.ArrayAssignmentStatementContext): Option[ASTNode] = {
    for (
      idRaw <- Option(ctx.IDENTIFIER());
      id = Identifier(idRaw.getSymbol.getText);
      indExpRaw <- Option(ctx.expression(0));
      indExp <- indExpRaw.accept(this).asInstanceOf[Option[Expression]];
      valExpRaw <- Option(ctx.expression(1));
      valExp <- valExpRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine)
    ) yield ArrayAssignmentStatement(id, indExp, valExp, lineNumber)
  }

  override def visitBinaryOperationExpression(ctx: MiniJavaParser.BinaryOperationExpressionContext): Option[ASTNode] = {
    for (
      fstExpRaw <- Option(ctx.expression(0));
      fstExp <- fstExpRaw.accept(this).asInstanceOf[Option[Expression]];
      opRaw <- Option(ctx.BINARY_OPERATOR());
      op = opRaw.getSymbol.getText match {
        case "&&" => And
        case "<" => LessThan
        case "+" => Plus
        case "-" => Minus
        case "*" => Times
      };
      sndExpRaw <- Option(ctx.expression(1));
      sndExp <- sndExpRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield BinaryOperationExpression(fstExp, op, sndExp, lineNumber, columnNumber)
  }

  override def visitArrayAccessExpression(ctx: MiniJavaParser.ArrayAccessExpressionContext): Option[ASTNode] = {
    for (
      arrExpRaw <- Option(ctx.expression(0));
      arrExp <- arrExpRaw.accept(this).asInstanceOf[Option[Expression]];
      indExpRaw <- Option(ctx.expression(1));
      indExp <- indExpRaw.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield ArrayAccessExpression(arrExp, indExp, lineNumber, columnNumber)
  }

  override def visitArrayLengthExpression(ctx: MiniJavaParser.ArrayLengthExpressionContext): Option[ASTNode] = {
    for (
      expRaw <- Option(ctx.expression());
      exp <- expRaw.accept(this).asInstanceOf[Option[Expression]]
    ) yield ArrayLengthExpression(exp)
  }

  override def visitMethodCallExpression(ctx: MiniJavaParser.MethodCallExpressionContext): Option[ASTNode] = {
    for (
      objExpRaw <- Option(ctx.objectExpression);
      objExp <- objExpRaw.accept(this).asInstanceOf[Option[Expression]];
      mId <- Option(ctx.IDENTIFIER());
      methodName = Identifier(mId.getSymbol.getText);
      paramsRaw <- Option(ctx.expression());
      params <- paramsRaw.toArray.drop(1)
        .map(_.asInstanceOf[MiniJavaParser.ExpressionContext].accept(this).asInstanceOf[Option[Expression]])
        .toList.sequence;
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield MethodCallExpression(objExp, methodName, params, lineNumber, columnNumber)
  }

  override def visitIntegerLiteral(ctx: MiniJavaParser.IntegerLiteralContext): Option[ASTNode] = {
    for (
      i <- Option(ctx.INTEGER_LITERAL())
    ) yield IntegerLiteral(Integer.parseInt(i.getSymbol.getText))
  }

  override def visitTrue(ctx: MiniJavaParser.TrueContext): Option[ASTNode] = {
    Some(TrueLiteral)
  }

  override def visitFalse(ctx: MiniJavaParser.FalseContext): Option[ASTNode] = {
    Some(FalseLiteral)
  }

  override def visitIdentifierExpression(ctx: MiniJavaParser.IdentifierExpressionContext): Option[ASTNode] = {
    for (
      id <- Option(ctx.IDENTIFIER());
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield IdentifierExpression(Identifier(id.getSymbol.getText), lineNumber, columnNumber)
  }

  override def visitThis(ctx: MiniJavaParser.ThisContext): Option[ASTNode] = {
    Some(ThisLiteral)
  }

  override def visitNewIntArrayExpression(ctx: MiniJavaParser.NewIntArrayExpressionContext): Option[ASTNode] = {
    for (
      e <- Option(ctx.expression());
      exp <- e.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield NewIntArrayExpression(exp, lineNumber, columnNumber)
  }

  override def visitNewObjectExpression(ctx: MiniJavaParser.NewObjectExpressionContext): Option[ASTNode] = {
    for (
      id <- Option(ctx.IDENTIFIER());
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield NewObjectExpression(Identifier(id.getSymbol.getText), lineNumber, columnNumber)
  }

  override def visitNegationExpression(ctx: MiniJavaParser.NegationExpressionContext): Option[ASTNode] = {
    for (
      e <- Option(ctx.expression());
      exp <- e.accept(this).asInstanceOf[Option[Expression]];
      lineNumber <- Option(ctx.start.getLine);
      columnNumber <- Option(ctx.start.getCharPositionInLine)
    ) yield NegatedExpression(exp, lineNumber, columnNumber)
  }

  override def visitParenedExpression(ctx: MiniJavaParser.ParenedExpressionContext): Option[ASTNode] = {
    for (
      e <- Option(ctx.expression());
      exp <- e.accept(this).asInstanceOf[Option[Expression]]
    ) yield ParenedExpression(exp)
  }
}
