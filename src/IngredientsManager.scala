import scala.io.StdIn.readLine
import MenuManager._

class Ingredient(name: String, category: String){
    val _name: String = name
    val _category: String = category
    // val _diets: List[String] = d
}

object IngredientsManager {

    def dbIngredients: List[Ingredient] = List(
        new Ingredient("Beef", "Meats"),
        new Ingredient("Chicken Breast", "Meats"),
        new Ingredient("Meatballs", "Meats"),
        new Ingredient("Carrots", "Vegetables"),
        new Ingredient("Tomatoes", "Vegetables"),
        new Ingredient("Potatoes", "Vegetables"),
        new Ingredient("Salt", "Spices"),
        new Ingredient("Pepper", "Spices"),
        new Ingredient("Sugar", "Spices"),
    )

    def getCategories(ingredients: List[Ingredient]): List[String] = {
        ingredients match {
            case Nil => Nil;
            case x :: ls => x._category :: getCategories(ls);
        }
    }

    def showCategories() = {
        def categories = getCategories(dbIngredients).distinct.sorted;
        categories.foreach(s => println(s));
    }

    def showIngredients(category: String) =
        dbIngredients.foreach(i => {
            if (i._category == category) println(i._name)
        });

    def addIngredient(myIngredients: List[Ingredient]): List[Ingredient] = {
        showCategories();
        val category = askForStringInput("Choose a category: ");
        showIngredients(category);
        val ingredientName = askForStringInput("Choose an ingredient: ");
        val foundIngredient: Ingredient = dbIngredients.find(ingredient => ingredient._name == ingredientName).get: Ingredient;
        myIngredients.::(foundIngredient);
    }

    def showMyIngredients(myIngredients: List[Ingredient]) =
    {
        def printIngredients(myIngredients: List[Ingredient], i: Int): String =
            myIngredients match {
                case Nil => "";
                case ing :: ingredients => i + ") " + ing._name + "\n" + printIngredients(ingredients, i+1)
            }

        println(printIngredients(myIngredients, 1));
    };

    def removeIngredient(myIngredients: List[Ingredient]): List[Ingredient] = {
        def removeAtIndex(myIngredients: List[Ingredient], index: Int, acc: Int): List[Ingredient] =
            myIngredients match {
                case Nil => Nil;
                case ing :: ingredients => if(acc == index) removeAtIndex(ingredients, index, acc + 1) else ing :: removeAtIndex(ingredients, index, acc + 1);
            }

        showMyIngredients(myIngredients);
        val ingredientToRemove = askForStringInput("Enter ingredient number: ").toInt;
        removeAtIndex(myIngredients, ingredientToRemove, 1);
    }

    def mainLoop(myIngredients: List[Ingredient]): Any = {
        val i = showMenu(List("Add Ingredients", "Remove Ingredients", "Show My Ingredients"));

        i match {
            case 0 => sys.exit();
            case 1 => mainLoop(addIngredient(myIngredients));
            case 2 => mainLoop(removeIngredient(myIngredients));
            case 3 => {
                showMyIngredients(myIngredients); mainLoop(myIngredients);
            }
            case _ => println("Error: Please provide a valid input!");
        }
    }

    def main(args: Array[String]): Unit = {
        mainLoop(List[Ingredient]());
    }
}