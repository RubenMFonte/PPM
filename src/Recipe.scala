import scala.Console.println
import scala.util.Random
import MenuManager._
import scala.annotation.tailrec


case class Recipe(recipeName: String, ingredientsList: List[Ingredient], dietTypeList: List[String], difficultyLevel: String, commentsList: List[String], rating: Int) extends Serializable;

object Recipe {

  def dbRecipes: List[Recipe] = List(
    new Recipe("Bacalhau à Brás", List(new Ingredient("Bacalhau", "Fish", 100), new Ingredient("Batata", "Vegetables", 20), new Ingredient("Cebola", "Vegetables", 14)), List("Pescetarianismo","Carnivora"), "easy",List(""),0),
    new Recipe("Strogonoff de Frango", List(new Ingredient("Frango", "Meat", 150), new Ingredient("Natas", "Lacticinios", 150), new Ingredient("Cebola", "Vegetables", 12)), List("Pescetarianismo","Carnivora"), "medium",List(""),0),
    new Recipe("Arroz de Pato", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Pato", "Lacticinios", 234), new Ingredient("Cebola", "Vegetables",12)), List("Vegetariano","Carnivora"), "hard",List(""),0),
    new Recipe("Arroz de Frango", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Massa de Frango", List(new Ingredient("Massa", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Sopa de Frango", List(new Ingredient("Agua", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
  )

  def compareFilterWithListOnRecipe[T](filter: T, listOnRecipe: List[T], compareElements: (T,T) => Boolean): Boolean = {
    listOnRecipe match {
      case Nil => false;
      case x :: xs => if (compareElements(filter, x)) true
      else compareFilterWithListOnRecipe(filter, xs, compareElements);
    }
  }

  def recipeFilterSingleElement[T](recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filter: T)(reversed: Boolean) =
    recipeFilter(recipeList, getListOnRecipe, compareElements, List(filter))(reversed);

  // General function to filter a list of some element on the recipe with a list of filters
  def recipeFilter[T](recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filterList: List[T])(reversed: Boolean): List[Recipe] = {

    // Check if the list of the recipe contains the list of filters
    def compareLists(listOnRecipe: List[T], filterList: List[T], compareElements: (T,T) => Boolean, reversed: Boolean): Boolean = {
      def evaluateComparison(comparison: Boolean, reversed: Boolean): Boolean = {
        if(reversed) comparison else !comparison;
      }

      filterList match {
        case Nil => true;
        case f :: fl => if(evaluateComparison(compareFilterWithListOnRecipe(f, listOnRecipe, compareElements), reversed)) false
        else compareLists(listOnRecipe, fl, compareElements, reversed);
      }
    }

    recipeList match {
      case Nil => Nil;
      case r :: rl => if( compareLists( getListOnRecipe(r), filterList, compareElements, reversed) ) r :: recipeFilter(rl, getListOnRecipe, compareElements, filterList)(reversed)
                      else recipeFilter(rl, getListOnRecipe, compareElements, filterList)(reversed);
    }
  }

  def recipeFilterNormal[T](recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filterList: List[T]) =
    recipeFilter(recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filterList: List[T])(false);

  def recipeFilterReversed[T](recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filterList: List[T]) =
    recipeFilter(recipeList: List[Recipe], getListOnRecipe: Recipe => List[T], compareElements: (T,T) => Boolean, filterList: List[T])(true);

  def compareStrings(s1: String, s2: String): Boolean = s1.equals(s2);

  def filterRecipesByDiets(recipeList: List[Recipe], dietList: List[String]): List[Recipe] = {
    def getRecipeDietList(recipe: Recipe): List[String] = recipe.dietTypeList;
    recipeFilterNormal(recipeList, getRecipeDietList, compareStrings, dietList);
  }

  def getRecipeIngredientsList(recipe: Recipe): List[Ingredient] = recipe.ingredientsList;
  def compareIngredients(i1: Ingredient, i2: Ingredient): Boolean = i1._name.equals(i2._name);

  def filterRecipesByIngredients(recipeList: List[Recipe], ingredientsList: List[Ingredient]): List[Recipe] = {
    recipeFilterNormal(recipeList, getRecipeIngredientsList, compareIngredients, ingredientsList);
  }

  def filterRecipesByProhibitedIngredients(recipeList: List[Recipe], prohibitedIngredientList: List[Ingredient]): List[Recipe] = {
    recipeFilterReversed(recipeList, getRecipeIngredientsList, compareIngredients, prohibitedIngredientList);
  }

  def filterRecipesByDifficultyLevel(recipeList: List[Recipe], difficultyLevel: String): List[Recipe] = {
    def getRecipeDifficultyLevel(recipe: Recipe): List[String] = List(recipe.difficultyLevel);
    recipeFilterSingleElement(recipeList, getRecipeDifficultyLevel, compareStrings, difficultyLevel)(false);
  }

  def getRecipesNamesAsList(recipes: List[Recipe]): List[String] = {
    def getRecipeName(recipe: Recipe) = recipe.recipeName;
    MenuManager.getNameList(recipes, getRecipeName);
  }

  def getRecipeCalories(recipe: Recipe): Int = {
    Ingredient.getCaloriesFromIngredientsList(recipe.ingredientsList);
  }

  def filterRecipesByCalories(recipeList: List[Recipe], minCalories: Int, maxCalories: Int): List[Recipe] = {
    def getRecipeCaloricRange(recipe: Recipe): List[(Int, Int)] = {
      val calories = getRecipeCalories(recipe);
      List((calories,calories));
    }
    def compareCalories(rangeOfCalories: (Int,Int), recipeCalories: (Int, Int)): Boolean = recipeCalories._1 > rangeOfCalories._1 && recipeCalories._1 < rangeOfCalories._2;
    recipeFilterSingleElement(recipeList, getRecipeCaloricRange, compareCalories, (minCalories, maxCalories))(false);
  }

  def getIngredientsFromRecipeList(recipeList: List[Recipe]): List[Ingredient] = {
    recipeList match {
      case Nil => Nil
      case x :: xs => x.ingredientsList ++ getIngredientsFromRecipeList(xs)
    }
  }

  def suggestRecipes(recipesList: List[Recipe]): List[Recipe] = {
    Random.shuffle(recipesList).take(5);
  }

  def getCaloriesFromRecipeList(recipeList: List[Recipe]): Int = {
    recipeList match {
      case Nil => 0
      case x :: xs => getRecipeCalories(x) + getCaloriesFromRecipeList(xs)
    }
  }

  def showMyRecipes(myRecipes: List[Recipe]) = {
    MenuManager.showMenu(getRecipesNamesAsList(myRecipes));
  };


}
/*
object RecipesManager {

  // QUAL A RAZAO PARA ESTA FUNCAO?
  def getRecipeIngredients(recipe: Recipe): List[Ingredient] = {
    recipe.ingredientsList
  }

  //--------------- Receive list of Ingredients from the choosen Recipes ----------------//
  // QUAL A RAZAO PARA ESTAS FUNCOES?

  def getIngredientsFromRecipeList(recipeList: List[Recipe]): List[Ingredient] = {
    recipeList match {
      case Nil => Nil
      case x :: xs => x.ingredientsList ++ getIngredientsFromRecipeList(xs)
    }
  }

  def getNameList_IngredientsFromRecipeList(ingredientsList: List[Ingredient]): List[String] = {
    ingredientsList match {
      case Nil => Nil
      case x :: xs => x._name :: getNameList_IngredientsFromRecipeList(xs)
    }
  }

  def removeDuplicates_getIngredientsFromRecipeList(ingredientsNameList: List[String]): List[String] = {
    ingredientsNameList match {
      case Nil => Nil
      case _ => ingredientsNameList.distinct
    }
  }

  def getShoppingList(mealMenu: MealMenu): Unit = {
    println(removeDuplicates_getIngredientsFromRecipeList(getNameList_IngredientsFromRecipeList((getIngredientsFromRecipeList(mealMenu.recipesList)))))



 /* def main(args: Array[String]): Unit = {
    //mainLoopRecipe(List());
    println("Início")
    //showMyRecipes(getRecipesByDiet(dbRecipes, List("Pescetarianismo","Carnivora")))
    //showMyRecipes(getRecipesByIngredient(dbRecipes, List(new Ingredient("Frango", "Meats"),new Ingredient("Bacalhau", "Fish"))))
    //showMyRecipes(getRecipesByProhibitedIngredient(dbRecipes, List(new Ingredient("Frango", "Meats"),new Ingredient("Bacalhau", "Fish"))))
    //showMyRecipes(getRecipesByDifficultyLevel(dbRecipes, "hard"))
    //showListStrings(removeDuplicates_getIngredientsFromRecipeList(getNameList_IngredientsFromRecipeList(getIngredientsFromRecipeList(dbRecipes))))
    //println(List(1,2,3,4,5).take(2))
    //println( showMyRecipes(getMenu(1070, dbRecipes, 2, 1).recipesList ) )
    //showMyRecipes(suggestRecipes(dbRecipes))
    //accessRecipe(new Recipe("Bacalhau à Brás", List(new Ingredient("Bacalhau", "Fish"), new Ingredient("Batata", "Vegetables"), new Ingredient("Cebola", "Vegetables")), List("Pescetarianismo","Carnivora"), "easy"))
    //showMyRecipes( suggestMenu(123,dbRecipes,1,2).recipesList )
    //getShoppingList(suggestMenu(123,dbRecipes,1,2))
    //println(getCaloriesByMealMenu(new MealMenu(123, List(new Recipe("Bacalhau à Brás", List(new Ingredient("Bacalhau", "Fish", 100), new Ingredient("Batata", "Vegetables", 20), new Ingredient("Cebola", "Vegetables", 14)), List("Pescetarianismo","Carnivora"), "easy")), 1, 1)))
    //showMyRecipes(getRecipesByCalories(dbRecipes, 0, 800))
    println("Fim")
  }

  def subMainLoop(myRecipes: List[Recipe]): Any = {
    val i = showMenu(getRecipeName(myRecipes));
    i match {
      case 0 => sys.exit();
      case 1 => accessRecipe(myRecipes(0))
      case 2 => accessRecipe(myRecipes(1))
      case 3 => accessRecipe(myRecipes(2))
      case 4 => accessRecipe(myRecipes(3))
      case 5 => accessRecipe(myRecipes(4))
      case _ => println("Error: Please provide a valid input!");
    }
  }


  def mainLoop(myRecipes: List[Recipe]): Any = {
    val i = showMenu(List("Sugestão de Receitas", "Remove Ingredients", "Show My Ingredients"));

    i match {
      case 0 => sys.exit();
      case 1 => subMainLoop(suggestRecipes(dbRecipes))
      /*    case 2 => mainLoop(removeIngredient(myIngredients));
          case 3 => {
            showMyIngredients(myIngredients); mainLoop(myIngredients);
         } */
      case _ => println("Error: Please provide a valid input!");
    }
  }

  def main(args: Array[String]): Unit = {
    mainLoop(List[Recipe]());
  }*/

}*/
