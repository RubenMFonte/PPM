import scala.util.Random

case class MealMenu ( idMealMenu: Int, recipesList: List[Recipe], numberOfDays: Int, numberOfMealsPerDay: Int ) extends Serializable;

object MealMenu {

  def createMenu(idMealMenu: Int, recipesList: List[Recipe], numberOfDays: Int, numberOfMealsPerDay: Int): MealMenu =
    new MealMenu(idMealMenu, Random.shuffle(recipesList).take(numberOfDays * numberOfMealsPerDay), numberOfDays, numberOfMealsPerDay);

  def getCaloriesByMealMenu(mealMenu: MealMenu): Int = Recipe.getCaloriesFromRecipeList(mealMenu.recipesList);
}
