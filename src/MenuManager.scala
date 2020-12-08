import scala.io.StdIn.readLine;

object MenuManager {

  def showMenu(options: List[String]): Int = {
    def showOptions(options: List[String], acc: Int): String = {
      options match {
        case Nil => "0) Exit";
        case s :: ops => acc + ") " + s + ", " + showOptions(ops, acc + 1);
      }
    }

    println(showOptions(options, 1));
    readLine().toInt;
  }

  def showOptions(options: List[String]): Int = {
    def showOptions(options: List[String], acc: Int): String = {
      options match {
        case Nil => "";
        case s :: ops => acc + ") " + s + ", " + showOptions(ops, acc + 1);
      }
    }

    println(showOptions(options, 1));
    readLine().toInt;
  }

  def askForStringInput(question: String): String = {
    println(question);
    readLine();
  }

  def getNameList[T](objects: List[T], getName: T => String): List[String] = {
    objects match {
      case Nil => Nil;
      case obj :: objectsList => getName(obj) :: getNameList(objectsList, getName);
    }
  }

  def printObjectsOnScreen[T](objectList: List[T], asString: T => String): Unit = {
    objectList match {
      case Nil => Nil;
      case x :: xs => println(asString(x)); printObjectsOnScreen(xs, asString);
    }
  }

  def printStringsOnScreen[T](strings: List[String]): Unit = {
    strings match {
      case Nil => Nil;
      case x :: xs => println(x); printStringsOnScreen(xs);
    }
  }

  def printRecipe(recipe: Recipe) = {
    println("Recipe - " + recipe.recipeName)
    println("Ingredients: ");
    printObjectsOnScreen(recipe.ingredientsList, Ingredient.getIngredientName);
    println("Diets: ");
    printStringsOnScreen(recipe.dietTypeList);
    println("Difficulty: " + recipe.difficultyLevel)
    println("Calories: " + Recipe.getRecipeCalories(recipe))
  }
}
