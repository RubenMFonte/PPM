import scala.Console.println
import scala.util.Random
import MenuManager._


class Recipe ( recipeNameArg: String, ingredientsListArg: List[Ingredient], dietTypeListArg: List[String], difficultyLevelArg: String, commentsListArg: List[String], ratingArg: Int ){
  val recipeName: String = recipeNameArg
  val ingredientsList: List[Ingredient] = ingredientsListArg
  val dietTypeList: List[String] = dietTypeListArg
  val difficultyLevel: String = difficultyLevelArg
  val commentsList: List[String] = commentsListArg
  val rating: Int = ratingArg
}

class MealMenu ( idMealMenuArg: Int, recipesListArg: List[Recipe], numberOfDaysArg: Int, numberOfMealsPerDayArg: Int ){
  val idMealMenu: Int = idMealMenuArg
  val recipesList: List[Recipe] = recipesListArg
  val numberOfDays: Int = numberOfDaysArg
  val numberOfMealsPerDay: Int = numberOfMealsPerDayArg
}

object RecipesManager {

  def dbRecipes: List[Recipe] = List(
    new Recipe("Bacalhau à Brás", List(new Ingredient("Bacalhau", "Fish", 100), new Ingredient("Batata", "Vegetables", 20), new Ingredient("Cebola", "Vegetables", 14)), List("Pescetarianismo","Carnivora"), "easy",List(""),0),
    new Recipe("Strogonoff de Frango", List(new Ingredient("Frango", "Meat", 150), new Ingredient("Natas", "Lacticinios", 150), new Ingredient("Cebola", "Vegetables", 12)), List("Pescetarianismo","Carnivora"), "medium",List(""),0),
    new Recipe("Arroz de Pato", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Pato", "Lacticinios", 234), new Ingredient("Cebola", "Vegetables",12)), List("Vegetariano","Carnivora"), "hard",List(""),0),
    new Recipe("Arroz de Frango", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Massa de Frango", List(new Ingredient("Massa", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Sopa de Frango", List(new Ingredient("Agua", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),

  )

  def getRecipeName(recipeList: List[Recipe]): List[String] = {
    recipeList match {
      case Nil => Nil;
      case x :: ls => x.recipeName :: getRecipeName(ls);
    }
  }

  def getRecipeIngredients(recipe: Recipe): List[Ingredient] = {
    recipe.ingredientsList
  }



  //--------------- Filter Recipes List from diet list ----------------//

  //Verify if a list of diets from one recipe contains a certain diet
  def dietListContainsDiet(dietTypeList: List[String], dietType: String): Boolean = {
    dietTypeList match {
      case Nil => false
      case x :: xs => if(x.equals(dietType)) {
        true
      }
      else dietListContainsDiet(xs, dietType)
    }
  }

  //Verify if one recipe contains the dietTypeList
  def recipeContainDiets(recipe: Recipe, dietTypeListArg: List[String] ): Boolean = {
    dietTypeListArg match {
      case Nil => false
      case x :: xs => if(dietListContainsDiet(recipe.dietTypeList,x)) true
      else recipeContainDiets(recipe, xs)
    }
  }

  def getRecipesByDiet(recipeList: List[Recipe], dietList: List[String]): List[Recipe] = {
    recipeList match {
      case Nil => recipeList
      case x :: xs => if (recipeContainDiets(x, dietList)) x :: getRecipesByDiet(xs, dietList)
      else getRecipesByDiet(xs, dietList)
    }
  }


  //--------------- Filter Recipes List from ingredient list ----------------//

  //Verify if a list of diets from one recipe contains a certain diet
  def ingredientListContainsIngredient(ingredientsList: List[Ingredient], ingredient: Ingredient): Boolean = {
    ingredientsList match {
      case Nil => false
      case x :: xs => if(x._name.equals(ingredient._name)) true
      else ingredientListContainsIngredient(xs, ingredient)
    }
  }

  //Verify if one recipe contains the ingredientList
  def recipeContainIngredients(recipe: Recipe, ingredientListArg: List[Ingredient] ): Boolean = {
    ingredientListArg match {
      case Nil => false
      case x :: xs => if(ingredientListContainsIngredient(recipe.ingredientsList,x)) true
      else recipeContainIngredients(recipe, xs)
    }
  }

  def getRecipesByIngredient(recipeList: List[Recipe], ingredientList: List[Ingredient]): List[Recipe] = {
    recipeList match {
      case Nil => recipeList
      case x :: xs => if (recipeContainIngredients(x, ingredientList)) x :: getRecipesByIngredient(xs, ingredientList)
      else getRecipesByIngredient(xs, ingredientList)
    }
  }

  //--------------- Filter Recipes List from prohibited ingredient list ----------------//

  def getRecipesByProhibitedIngredient(recipeList: List[Recipe], prohibitedIngredientList: List[Ingredient]): List[Recipe] = {
    recipeList match {
      case Nil => recipeList
      case x :: xs => if (!recipeContainIngredients(x, prohibitedIngredientList)) x :: getRecipesByProhibitedIngredient(xs, prohibitedIngredientList)
      else getRecipesByProhibitedIngredient(xs, prohibitedIngredientList)
    }
  }

  //--------------- Filter Recipes List from Level of Difficulty ----------------//

  def getRecipesByDifficultyLevel(recipeList: List[Recipe], difficultyLevel: String): List[Recipe] = {
    recipeList match {
      case Nil => recipeList
      case x :: xs => if (x.difficultyLevel.equals(difficultyLevel)) x :: getRecipesByDifficultyLevel(xs, difficultyLevel)
      else getRecipesByDifficultyLevel(xs, difficultyLevel)
    }
  }

  //--------------- Filter Recipes List with min and max Calories ----------------//

  def getRecipesByCalories(recipeList: List[Recipe], minCalories: Int, maxCalories: Int): List[Recipe] = {
    recipeList match {
      case Nil => recipeList
      case x :: xs => if ( minCalories <= getCaloriesByRecipe(x) && getCaloriesByRecipe(x) <= maxCalories )   x :: getRecipesByCalories(xs, minCalories, maxCalories)
      else getRecipesByCalories(xs, minCalories, maxCalories)
    }
  }

  //--------------- Receive list of Ingredients from the choosen Recipes ----------------//

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

                    //--------------- Suggest week menu ----------------//

  def suggestMenu(idMealMenuArg: Int, recipesListArg: List[Recipe], numberOfDaysArg: Int, numberOfMealsPerDayArg: Int): MealMenu = {
    //Random.shuffle(recipesListArg).take(numberOfDaysArg * numberOfMealsPerDayArg)
    new MealMenu(idMealMenuArg, Random.shuffle(recipesListArg).take(numberOfDaysArg * numberOfMealsPerDayArg), numberOfDaysArg, numberOfMealsPerDayArg )
  }

                    //--------------- Suggest recipes ----------------//
  def suggestRecipes(recipesListArg: List[Recipe]): List[Recipe] = {
    Random.shuffle(recipesListArg).take(5)
  }
                    //--------------- Access the Recipe ----------------//
  def accessRecipe(recipe: Recipe): Unit = {
    println("Recipe: " + recipe.recipeName)
    recipe.ingredientsList.foreach(i => println(i._name))
    recipe.dietTypeList.foreach(i => println(i))
    println("Difficulty: " + recipe.difficultyLevel)
    println("Calories: " + getCaloriesByRecipe(recipe))
  }
                  //--------------- Get Shopping List from MealMenu ----------------//
  def getShoppingList(mealMenu: MealMenu): Unit = {
    println(removeDuplicates_getIngredientsFromRecipeList(getNameList_IngredientsFromRecipeList((getIngredientsFromRecipeList(mealMenu.recipesList)))))
  }

                  //--------------- Get Calories from Recipe ----------------//
  def getCaloriesFromIngredientsList(ingredientsList: List[Ingredient]): Int = {
    ingredientsList match {
      case Nil => 0
      case x :: xs => x._calories + getCaloriesFromIngredientsList(xs)
    }
  }

  def getCaloriesByRecipe(recipe: Recipe): Int = {
    getCaloriesFromIngredientsList(recipe.ingredientsList)
  }

                 //--------------- Get Calories from MealMenu ----------------//
  def getCaloriesFromRecipeList(recipeList: List[Recipe]): Int = {
    recipeList match {
      case Nil => 0
      case x :: xs => getCaloriesByRecipe(x) + getCaloriesFromRecipeList(xs)
    }
  }

  def getCaloriesByMealMenu(mealMenu: MealMenu): Int = {
    getCaloriesFromRecipeList(mealMenu.recipesList)
  }


  //--------------- Main ----------------//

  def showListStrings(myStringsList: List[String]) =
    myStringsList.foreach(i => println(i));

 /* def showMyRecipes(myRecipes: List[Recipe]) =
    myRecipes.foreach(i => println(i.recipeName));
  */

  def showMyRecipes(myRecipes: List[Recipe]) =
  {
    def printRecipes(myRecipes: List[Recipe], i: Int): String =
      myRecipes match {
        case Nil => "";
        case x :: xs => i + ") " + x.recipeName + "\n" + printRecipes(xs, i+1)
      }

    println(printRecipes(myRecipes, 1));
  };

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
  }*/

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
  }

}
