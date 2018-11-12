package minijava.typechecking

import minijava.Main
import minijava.messages.{CompilerMessage, _}
import org.scalatest._

class TypeCheckingSpec extends FlatSpec with Matchers {
  "typeCheck" should "fail when multiple classes are declared with the same name" in {
    expectTypeCheckErrors(
      "examples/ClassNameDuplicate.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(20)), "Duplicate declaration of class \"Fac\""),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(33)), "Duplicate declaration of class \"Fac\"")
      )
    )
  }

  it should "fail when a class is declared with the same name as the main class" in {
    expectTypeCheckErrors(
      "examples/ClassDuplicateOfMain.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(7)), "Duplicate declaration of class \"Factorial\"")
      )
    )
  }

  it should "fail when a class extends itself" in {
    expectTypeCheckErrors(
      "examples/RecursiveInheritingClass.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class A, repeating at class A")
      )
    )
  }

  it should "fail when a set of classes form a cyclic inheritance" in {
    expectTypeCheckErrors(
      "examples/RecursiveInheritingClassCycle.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class A, repeating at class A"),
        CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class B, repeating at class B"),
        CompilerMessage(CompilerError, TypeCheckingError, None, "Circular inheritance found for class C, repeating at class C")
      )
    )
  }

  it should "fail when classes extend class that do not exist" in {
    expectTypeCheckErrors(
      "examples/ExtendClassDoesNotExist.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Class \"A\" extends unknown class \"Foo\"."),
        CompilerMessage(CompilerError, TypeCheckingError, None, "Class \"C\" extends unknown class \"Bar\".")
      )
    )
  }

  it should "report multiple type checking errors when multiple are present" in {
    expectTypeCheckErrors(
      "examples/MultTypeErrors.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Incompatible types in assignment statement for variable \"num_aux\".\nExpected:  int\nFound:     boolean"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(15)), "Incompatible types in assignment statement for variable \"num_aux2\".\nExpected:  boolean\nFound:     int"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(17, 18)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
      )
    )
  }

  it should "fail when a type check error exists in the main method" in {
    expectTypeCheckErrors(
      "examples/HelloTrue.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(3)), "Incompatible types in print statement.\nExpected:  int\nFound:     boolean")
      )
    )
  }

  it should "fail when a non-boolean value is used in an if statement conditional" in {
    expectTypeCheckErrors(
      "examples/IfStatementConditionTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Incompatible types in if statement condition.\nExpected:  boolean\nFound:     int")
      )
    )
  }

  it should "fail when a non-boolean value is used in a while loop conditional" in {
    expectTypeCheckErrors(
      "examples/WhileLoopConditionTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(16)), "Incompatible types in while statement condition.\nExpected:  boolean\nFound:     int")
      )
    )
  }

  it should "fail when incorrect types are used in a array assignment statement" in {
    expectTypeCheckErrors(
      "examples/ArrayAssignmentTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Incompatible types in array assignment index expression.\nExpected:  int\nFound:     boolean"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(15)), "Incompatible types in array assignment value expression.\nExpected:  int\nFound:     boolean"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(16)), "Incompatible types in array assignment statement.\nExpected:  int[]\nFound:     int")
      )
    )
  }

  it should "fail when incorrect types are used in a array access expression" in {
    expectTypeCheckErrors(
      "examples/ArrayAccessTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(17, 16)), "Incompatible types in array access index expression.\nExpected:  int\nFound:     boolean"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(18, 16)), "Incompatible types in array access expression.\nExpected:  int[]\nFound:     int")
      )
    )
  }

  it should "fail when an incorrect type is used for the length of a new int array" in {
    expectTypeCheckErrors(
      "examples/NewIntArrayTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(12, 19)), "Incompatible types in new int array expression.\nExpected:  int\nFound:     boolean")
      )
    )
  }

  it should "fail when an incorrect type is used in a negated expression" in {
    expectTypeCheckErrors(
      "examples/NegatedExpressionTypeError.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(14, 18)), "Incompatible types in negated expression.\nExpected:  boolean\nFound:     int")
      )
    )
  }

  it should "fail when an operator is used with a wrong parameter type" in {
    expectTypeCheckErrors(
      "examples/PlusBoolean.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Found instance of + with parameters of type \"int\" and \"boolean\". No version of the operation was found for this type combination.\n\nValid type combinations for this operator are:\n\n\"int\" and \"int\"")
      )
    )
  }

  it should "fail when a class variable is declared multiple times" in {
    expectTypeCheckErrors(
      "examples/DuplicateClassVariable.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(9)), "Duplicate declaration of class variable \"num_aux\" of class Fac"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(10)), "Duplicate declaration of class variable \"num_aux\" of class Fac")
      )
    )
  }

  it should "fail when a parameter is declared multiple times" in {
    expectTypeCheckErrors(
      "examples/DuplicateParameter.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac"),
        CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Duplicate declaration of parameter \"num\" in method ComputeFac of class Fac")
      )
    )
  }

  it should "fail when a local variable is declared twice" in {
    expectTypeCheckErrors(
      "examples/DuplicateLocalVariable.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Duplicate declaration of local variable \"num_aux\" in method ComputeFac of class Fac"),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(12)), "Duplicate declaration of local variable \"num_aux\" in method ComputeFac of class Fac")
      )
    )
  }

  it should "fail when a variable is used without being declared" in {
    expectTypeCheckErrors(
      "examples/MissingDeclaration.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(11)), "Unknown variable \"num_aux\", perhaps it has not been declared."),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineNumber(13)), "Unknown variable \"num_aux\", perhaps it has not been declared."),
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(14, 15)), "Unknown variable \"num_aux\", perhaps it has not been declared.")
      )
    )
  }

  it should "fail when an unknown method is called" in {
    expectTypeCheckErrors(
      "examples/UnknownMethod.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int)\" called on object of class \"Fac\", but that method is not defined for that class.")
      )
    )
  }

  it should "pass when a non-overrided method from a parent class is called" in {
    expectPassWithClasses(
      "examples/CallParentMethod.minijava",
      List("Factorial", "Fac", "A")
    )
  }

  it should "fail when a method is called on a non-object value" in {
    expectTypeCheckErrors(
      "examples/MethodCallNonObject.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac\" called on non-object value of type \"int\".")
      )
    )
  }

  it should "fail when an unknown class is instantiated" in {
    expectTypeCheckErrors(
      "examples/InstantiateUnknownClass.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Instantiation of unknown class \"Fac\".")
      )
    )
  }

  it should "fail when a method taking in an object is given an object of the wrong class" in {
    expectTypeCheckErrors(
      "examples/PassIncorrectClass.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int, B)\" called on object of class \"Fac\", but that method is not defined for that class.")
      )
    )
  }

  it should "fail when a method taking in an object is given a primitive" in {
    expectTypeCheckErrors(
      "examples/PassPrimitiveInsteadOfClass.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(3, 27)), "Method \"ComputeFac(int, int)\" called on object of class \"Fac\", but that method is not defined for that class.")
      )
    )
  }

  it should "pass the Factorial example" in {
    expectPassWithClasses(
      "examples/Factorial.minijava",
      List("Factorial", "Fac")
    )
  }

  it should "pass the BinarySearch example" in {
    expectPassWithClasses(
      "examples/BinarySearch.minijava",
      List("BinarySearch", "BS")
    )
  }

  it should "pass the BubbleSort example" in {
    expectPassWithClasses(
      "examples/BubbleSort.minijava",
      List("BubbleSort", "BBS")
    )
  }

  it should "pass the TreeVisitor example" in {
    expectPassWithClasses(
      "examples/TreeVisitor.minijava",
      List("TreeVisitor", "TV", "Tree", "Visitor", "MyVisitor")
    )
  }

  it should "pass the QuickSort example" in {
    expectPassWithClasses(
      "examples/QuickSort.minijava",
      List("QuickSort", "QS")
    )
  }

  it should "pass the LinearSearch example" in {
    expectPassWithClasses(
      "examples/LinearSearch.minijava",
      List("LinearSearch", "LS")
    )
  }

  it should "pass the LinkedList example" in {
    expectPassWithClasses(
      "examples/LinkedList.minijava",
      List("LinkedList", "Element", "List", "LL")
    )
  }

  it should "pass the BinaryTree example" in {
    expectPassWithClasses(
      "examples/BinaryTree.minijava",
      List("BinaryTree", "BT", "Tree")
    )
  }

  it should "pass the HelloOne example" in {
    expectPassWithClasses(
      "examples/HelloOne.minijava",
      List("HelloOne")
    )
  }

  it should "pass a program with valid method overloading" in {
    expectPassWithClasses(
      "examples/ValidOverloading.minijava",
      List("Factorial", "Fac")
    )
  }

  it should "fail when a method is overloaded on return type" in {
    expectTypeCheckErrors(
      "examples/InvalidOverloading.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Multiple methods \"ComputeFac(int)\" exist for class \"Fac\":\n\nboolean ComputeFac(int)\nint ComputeFac(int)\n\nNote that overloading based on return type is not allowed.")
      )
    )
  }

  it should "fail when a method is overloaded on return type via a parent class" in {
    expectTypeCheckErrors(
      "examples/InvalidOverloadingInherit.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Multiple methods \"ComputeFac(int)\" exist for class \"Fac\":\n\nboolean ComputeFac(int)\nint ComputeFac(int)\n\nNote that overloading based on return type is not allowed.")
      )
    )
  }

  it should "fail when a method declared multiple times for the same class" in {
    expectTypeCheckErrors(
      "examples/DuplicateMethodDeclaration.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Duplicate declaration(s) of method \"int ComputeFac(int)\" for class \"Fac\".")
      )
    )
  }

  it should "pass a program with covariant overriding" in {
    expectPassWithClasses(
      "examples/CovariantOverride.minijava",
      List("Factorial", "Fac", "A")
    )
  }

  it should "pass a program with multi-level covariant overriding" in {
    expectPassWithClasses(
      "examples/CovariantOverride2.minijava",
      List("Factorial", "Fac", "A", "B")
    )
  }

  it should "pass a program with multi-level covariant overriding where the last child does not override" in {
    expectPassWithClasses(
      "examples/CovariantOverride3.minijava",
      List("Factorial", "Fac", "A", "B")
    )
  }

  it should "fail when a local variable is declared with an unknown type" in {
    expectTypeCheckErrors(
      "examples/LocalVariableUnknownType.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"a\" for local variable \"num_2\" in method \"ComputeFac\" of class \"Fac\".")
      )
    )
  }

  it should "fail when a parameter is declared with an unknown type" in {
    expectTypeCheckErrors(
      "examples/ParameterUnknownType.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"A\" for parameter \"num\" in method \"hi\" of class \"Fac\".")
      )
    )
  }

  it should "fail when a class variable is declared with an unknown type" in {
    expectTypeCheckErrors(
      "examples/ClassVariableUnknownType.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Unknown type \"a\" for class variable \"num_2\" in class \"Fac\".")
      )
    )
  }

  it should "fail when an incorrect type is returned in a method" in {
    expectTypeCheckErrors(
      "examples/ReturnIncorrectType.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "Incorrect return type for method \"ComputeFac\" of class \"Fac\".\n\nExpected: int\nFound: boolean")
      )
    )
  }

  it should "fail when a method returns an unknown type" in {
    expectTypeCheckErrors(
      "examples/ReturnUnkownType.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, Some(LineColumn(9, 15)), "Instantiation of unknown class \"B\".")
      )
    )
  }

  it should "fail when a main method that has a print statement is not io" in {
    expectTypeCheckErrors(
      "examples/MainPrintNotIO.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO present in non-io method \"main\" of class \"MainPrintNotIO\".\n\nln:3\tPrint statement")
      )
    )
  }

  it should "fail when a main method that has an io method call is not io" in {
    expectTypeCheckErrors(
      "examples/MainNonIOCallsIO.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO present in non-io method \"main\" of class \"MainNonIOCallsIO\".\n\nln:4 col:16\tCall of io method \"doIO\"")
      )
    )
  }

  it should "fail when a class method that has a print statement is not io" in {
    expectTypeCheckErrors(
      "examples/MethodPrintNotIO.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO present in non-io method \"doIO\" of class \"A\".\n\nln:9\tPrint statement")
      )
    )
  }

  it should "fail when a class method that has an io method call is not io" in {
    expectTypeCheckErrors(
      "examples/MethodNotIOCallIOMethod.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO present in non-io method \"doNotDoIO\" of class \"A\".\n\nln:11 col:12\tCall of io method \"doIO\"")
      )
    )
  }

  it should "fail when a class method that has a return of an io method call is not io" in {
    expectTypeCheckErrors(
      "examples/MethodNonIOCallIOInReturn.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO present in non-io method \"doNotDoIO\" of class \"A\".\n\nln:9 col:15\tCall of io method \"doIO\"")
      )
    )
  }

  it should "give a warning when a main method is marked io, but contains no io" in {
    expectTypeCheckErrors(
      "examples/UnneccessaryIOMain.minijava",
      List(
        CompilerMessage(CompilerWarning, TypeCheckingError, None, "Method \"main\" of class \"UnneccessaryIOMain\" is marked as io, but does not perform any io.")
      )
    )
  }

  it should "pass when a class overrides an io method as non-io" in {
    expectPassWithClasses(
      "examples/OverrideIOWithNonIO.minijava",
      List("OverrideIOWithNonIO", "A", "B")
    )
  }

  it should "fail when a class overrides a non-io method as io" in {
    expectTypeCheckErrors(
      "examples/OverrideNonIOWithIO.minijava",
      List(
        CompilerMessage(CompilerError, TypeCheckingError, None, "IO method \"doNoIO\" of child class \"A\" overrides non-io method in parent class \"B\".")
      )
    )
  }

  it should "pass when a class overrides an io method as io" in {
    expectPassWithClasses(
      "examples/OverrideIOWithIO.minijava",
      List("OverrideIOWithIO", "A", "B")
    )
  }

  private def expectPassWithClasses(inputFile: String, expectedClasses: List[String]): Unit = {
    val input = Main.readFile(inputFile)

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult._2.isEmpty shouldBe true

    val typeTable = typeCheckResult._1.get

    expectedClasses.foreach(
      typeTable.get(_).isDefined shouldBe true
    )
  }

  private def expectTypeCheckErrors(inputFile: String, expected: List[CompilerMessage]): Unit = {
    val input = Main.readFile(inputFile)

    val ast = Main.parseString(input)

    ast.isRight shouldBe true

    val typeCheckResult = TypeChecking.typeCheck(ast.right.get)

    typeCheckResult._2.nonEmpty shouldBe true

    val errors = typeCheckResult._2.get

    errors shouldBe expected
  }
}
