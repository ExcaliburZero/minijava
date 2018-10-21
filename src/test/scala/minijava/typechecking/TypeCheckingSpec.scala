package minijava.typechecking

import minijava.Main
import minijava.messages.{CompilerMessage, _}
import org.scalatest._

class TypeCheckingSpec extends FlatSpec with Matchers {
  "typeCheck" should "fail when multiple classes are declared with the same name" in {
    val input = Main.readFile("examples/ClassNameDuplicate.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(20)), "Duplicate declaration of class \"Fac\""),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(33)), "Duplicate declaration of class \"Fac\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class is declared with the same name as the main class" in {
    val input = Main.readFile("examples/ClassDuplicateOfMain.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(7)), "Duplicate declaration of class \"Factorial\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class extends itself" in {
    val input = Main.readFile("examples/RecursiveInheritingClass.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class A, repeating at class A")
    )

    errors shouldBe expected
  }

  it should "fail when a set of classes form a cyclic inheritance" in {
    val input = Main.readFile("examples/RecursiveInheritingClassCycle.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class A, repeating at class A"),
      CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class B, repeating at class B"),
      CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class C, repeating at class C")
    )

    errors shouldBe expected
  }

  it should "fail when classes extend class that do not exist" in {
    val input = Main.readFile("examples/ExtendClassDoesNotExist.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Class \"A\" extends unknown class \"Foo\"."),
      CompilerMessage(CompilerError, TypeCheckingError, None, "Class \"C\" extends unknown class \"Bar\".")
    )

    errors shouldBe expected
  }

  it should "report multiple type checking errors when multiple are present" in {
    val input = Main.readFile("examples/MultTypeErrors.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Incompatible types in assignment statement for variable \"num_aux\".\nExpected:  int\nFound:     boolean"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(15)), "Incompatible types in assignment statement for variable \"num_aux2\".\nExpected:  boolean\nFound:     int"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(17, 18)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
    )

    errors shouldBe expected
  }

  it should "fail when a type check error exists in the main method" in {
    val input = Main.readFile("examples/HelloTrue.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(3)), "Incompatible types in print statement.\nExpected:  int\nFound:     boolean")
    )

    errors shouldBe expected
  }

  it should "fail when a non-boolean value is used in an if statement conditional" in {
    val input = Main.readFile("examples/IfStatementConditionTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Incompatible types in if statement condition.\nExpected:  boolean\nFound:     int")
    )

    errors shouldBe expected
  }

  it should "fail when a non-boolean value is used in a while loop conditional" in {
    val input = Main.readFile("examples/WhileLoopConditionTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(16)), "Incompatible types in while statement condition.\nExpected:  boolean\nFound:     int")
    )

    errors shouldBe expected
  }

  it should "fail when incorrect types are used in a array assignment statement" in {
    val input = Main.readFile("examples/ArrayAssignmentTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Incompatible types in array assignment index expression.\nExpected:  int\nFound:     boolean"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(15)), "Incompatible types in array assignment value expression.\nExpected:  int\nFound:     boolean"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(16)), "Incompatible types in array assignment statement.\nExpected:  int[]\nFound:     int")
    )

    errors shouldBe expected
  }

  it should "fail when incorrect types are used in a array access expression" in {
    val input = Main.readFile("examples/ArrayAccessTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(17, 16)), "Incompatible types in array access index expression.\nExpected:  int\nFound:     boolean"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(18, 16)), "Incompatible types in array access expression.\nExpected:  int[]\nFound:     int")
    )

    errors shouldBe expected
  }

  it should "fail when an incorrect type is used for the length of a new int array" in {
    val input = Main.readFile("examples/NewIntArrayTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(12, 19)), "Incompatible types in new int array expression.\nExpected:  int\nFound:     boolean")
    )

    errors shouldBe expected
  }

  it should "fail when an incorrect type is used in a negated expression" in {
    val input = Main.readFile("examples/NegatedExpressionTypeError.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(14, 18)), "Incompatible types in negated expression.\nExpected:  boolean\nFound:     int")
    )

    errors shouldBe expected
  }

  it should "fail when an operator is used with a wrong parameter type" in {
    val input = Main.readFile("examples/PlusBoolean.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
    )

    errors shouldBe expected
  }

  it should "fail when a class variable is declared multiple times" in {
    val input = Main.readFile("examples/DuplicateClassVariable.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(9)), "Duplicate declaration of class variable \"num_aux\" of class Fac"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(10)), "Duplicate declaration of class variable \"num_aux\" of class Fac")
    )

    errors shouldBe expected
  }

  it should "fail when a parameter is declared multiple times" in {
    val input = Main.readFile("examples/DuplicateParameter.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac"),
      CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac")
    )

    errors shouldBe expected
  }

  it should "fail when a local variable is declared twice" in {
    val input = Main.readFile("examples/DuplicateLocalVariable.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Duplicate declaration of local variable \"num_aux\" in method ComputeFac of class Fac"),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(12)), "Duplicate declaration of local variable \"num_aux\" in method ComputeFac of class Fac")
    )

    errors shouldBe expected
  }

  it should "fail when a variable is used without being declared" in {
    val input = Main.readFile("examples/MissingDeclaration.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Unknown variable \"num_aux\", perhaps it has not been declared."),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Unknown variable \"num_aux\", perhaps it has not been declared."),
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(14, 15)), "Unknown variable \"num_aux\", perhaps it has not been declared.")
    )

    errors shouldBe expected
  }

  it should "fail when an unknown method is called" in {
    val input = Main.readFile("examples/UnknownMethod.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int)\" called on object of class \"Fac\", but that method is not defined for that class.")
    )

    errors shouldBe expected
  }

  it should "pass when a non-overrided method from a parent class is called" in {
    val input = Main.readFile("examples/CallParentMethod.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
    typeTable.get("A").isDefined shouldBe true
  }

  it should "fail when a method is called on a non-object value" in {
    val input = Main.readFile("examples/MethodCallNonObject.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac\" called on non-object value of type \"int\".")
    )

    errors shouldBe expected
  }

  it should "fail when an unknown class is instantiated" in {
    val input = Main.readFile("examples/InstantiateUnknownClass.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Instantiation of unknown class \"Fac\".")
    )

    errors shouldBe expected
  }

  it should "fail when a method taking in an object is given an object of the wrong class" in {
    val input = Main.readFile("examples/PassIncorrectClass.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int, B)\" called on object of class \"Fac\", but that method is not defined for that class.")
    )

    errors shouldBe expected
  }

  it should "fail when a method taking in an object is given a primitive" in {
    val input = Main.readFile("examples/PassPrimitiveInsteadOfClass.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int, int)\" called on object of class \"Fac\", but that method is not defined for that class.")
    )

    errors shouldBe expected
  }

  it should "pass the Factorial example" in {
    val input = Main.readFile("examples/Factorial.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
  }

  it should "pass the BinarySearch example" in {
    val input = Main.readFile("examples/BinarySearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BinarySearch").isDefined shouldBe true
    typeTable.get("BS").isDefined shouldBe true
  }

  it should "pass the BubbleSort example" in {
    val input = Main.readFile("examples/BubbleSort.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BubbleSort").isDefined shouldBe true
    typeTable.get("BBS").isDefined shouldBe true
  }

  it should "pass the TreeVisitor example" in {
    val input = Main.readFile("examples/TreeVisitor.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("TreeVisitor").isDefined shouldBe true
    typeTable.get("TV").isDefined shouldBe true
    typeTable.get("Tree").isDefined shouldBe true
    typeTable.get("Visitor").isDefined shouldBe true
    typeTable.get("MyVisitor").isDefined shouldBe true
  }

  it should "pass the QuickSort example" in {
    val input = Main.readFile("examples/QuickSort.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("QuickSort").isDefined shouldBe true
    typeTable.get("QS").isDefined shouldBe true
  }

  it should "pass the LinearSearch example" in {
    val input = Main.readFile("examples/LinearSearch.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("LinearSearch").isDefined shouldBe true
    typeTable.get("LS").isDefined shouldBe true
  }

  it should "pass the LinkedList example" in {
    val input = Main.readFile("examples/LinkedList.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("LinkedList").isDefined shouldBe true
    typeTable.get("Element").isDefined shouldBe true
    typeTable.get("List").isDefined shouldBe true
    typeTable.get("LL").isDefined shouldBe true
  }

  it should "pass the BinaryTree example" in {
    val input = Main.readFile("examples/BinaryTree.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("BinaryTree").isDefined shouldBe true
    typeTable.get("BT").isDefined shouldBe true
    typeTable.get("Tree").isDefined shouldBe true
  }

  it should "pass the HelloOne example" in {
    val input = Main.readFile("examples/HelloOne.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("HelloOne").isDefined shouldBe true
  }

  it should "pass a program with valid method overloading" in {
    val input = Main.readFile("examples/ValidOverloading.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
  }

  it should "fail when a method is overloaded on return type" in {
    val input = Main.readFile("examples/InvalidOverloading.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Multiple methods \"ComputeFac(int)\" exist for class \"Fac\":\n\nboolean ComputeFac(int)\nint ComputeFac(int)\n\nNote that overloading based on return type is not allowed.")
    )

    errors shouldBe expected
  }

  it should "fail when a method is overloaded on return type via a parent class" in {
    val input = Main.readFile("examples/InvalidOverloadingInherit.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Multiple methods \"ComputeFac(int)\" exist for class \"Fac\":\n\nboolean ComputeFac(int)\nint ComputeFac(int)\n\nNote that overloading based on return type is not allowed.")
    )

    errors shouldBe expected
  }

  it should "fail when a method declared multiple times for the same class" in {
    val input = Main.readFile("examples/DuplicateMethodDeclaration.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration(s) of method \"int ComputeFac(int)\" for class \"Fac\".")
    )

    errors shouldBe expected
  }

  it should "pass a program with covariant overriding" in {
    val input = Main.readFile("examples/CovariantOverride.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
    typeTable.get("A").isDefined shouldBe true
  }

  it should "pass a program with multi-level covariant overriding" in {
    val input = Main.readFile("examples/CovariantOverride2.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
    typeTable.get("A").isDefined shouldBe true
    typeTable.get("B").isDefined shouldBe true
  }

  it should "pass a program with multi-level covariant overriding where the last child does not override" in {
    val input = Main.readFile("examples/CovariantOverride3.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isRight shouldBe true

    val typeTable = typeCheckResult.right.get

    typeTable.get("Factorial").isDefined shouldBe true
    typeTable.get("Fac").isDefined shouldBe true
    typeTable.get("A").isDefined shouldBe true
    typeTable.get("B").isDefined shouldBe true
  }

  it should "fail when a local variable is declared with an unknown type" in {
    val input = Main.readFile("examples/LocalVariableUnknownType.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"a\" for local variable \"num_2\" in method \"ComputeFac\" of class \"Fac\".")
    )

    errors shouldBe expected
  }

  it should "fail when a parameter is declared with an unknown type" in {
    val input = Main.readFile("examples/ParameterUnknownType.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"A\" for parameter \"num\" in method \"hi\" of class \"Fac\".")
    )

    errors shouldBe expected
  }

  it should "fail when a class variable is declared with an unknown type" in {
    val input = Main.readFile("examples/ClassVariableUnknownType.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"a\" for class variable \"num_2\" in class \"Fac\".")
    )

    errors shouldBe expected
  }

  it should "fail when an incorrect type is returned in a method" in {
    val input = Main.readFile("examples/ReturnIncorrectType.minijava")

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult.isLeft shouldBe true

    val errors = typeCheckResult.left.get

    val expected = List(
      CompilerMessage(CompilerError, TypeCheckingError, None, "Incorrect return type for method \"ComputeFac\" of class \"Fac\".\n\nExpected: int\nFound: boolean")
    )

    errors shouldBe expected
  }
}
