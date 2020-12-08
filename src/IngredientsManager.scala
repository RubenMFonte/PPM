import MenuManager._

case class Ingredient(_name: String, _category: String, _calories: Int) extends Serializable;

object Ingredient {

    def dbIngredients: List[Ingredient] = List(
        new Ingredient("Beef", "Meats", 100),
        new Ingredient("Chicken Breast", "Meats", 150),
        new Ingredient("Meatballs", "Meats", 220),
        new Ingredient("Carrots", "Vegetables", 30),
        new Ingredient("Tomatoes", "Vegetables", 20),
        new Ingredient("Potatoes", "Vegetables", 10),
        new Ingredient("Salt", "Spices", 45),
        new Ingredient("Pepper", "Spices", 1),
        new Ingredient("Sugar", "Spices", 20)
    )

    def getIngredientName(ingredient: Ingredient): String = ingredient._name;

    def getCaloriesFromIngredientsList(ingredientsList: List[Ingredient]): Int = {
        ingredientsList match {
            case Nil => 0
            case x :: xs => x._calories + getCaloriesFromIngredientsList(xs)
        }
    }

    def getIngredientsNamesAsList(ingredientsList: List[Ingredient]): List[String] = {
        def getIngredientName(ing: Ingredient) = ing._name;
        MenuManager.getNameList(ingredientsList, getIngredientName);
    }

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

    def addIngredient(myIngredients: List[String]): List[String] = {
        showCategories();
        val category = askForStringInput("Choose a category: ");
        showIngredients(category);
        val ingredientName = askForStringInput("Choose an ingredient: ");
        val foundIngredient: Ingredient = dbIngredients.find(ingredient => ingredient._name == ingredientName).get: Ingredient;
        myIngredients.::(foundIngredient._name);
    }

    def getMyIngredients(myIngredients: List[String], dbIngredients: List[Ingredient]): List[Ingredient] = {
        def getIngredientByName(name: String, dbIngredients: List[Ingredient]): Option[Ingredient] = {
            dbIngredients match {
                case Nil => None;
                case i :: il => if(i._name == name) Some(i) else getIngredientByName(name, il);
            }
        }
        myIngredients match {
            case Nil => Nil;
            case mi :: mil => getIngredientByName(mi, dbIngredients) match {
                case Some(ingredient) => ingredient :: getMyIngredients(mil, dbIngredients);
                case None => getMyIngredients(mil, dbIngredients);
            }
        }
    };

    def removeIngredient(myIngredients: List[String]): List[String] = {
        def removeAtIndex(myIngredients: List[String], index: Int, acc: Int): List[String] =
            myIngredients match {
                case Nil => Nil;
                case ing :: ingredients => if(acc == index) removeAtIndex(ingredients, index, acc + 1) else ing :: removeAtIndex(ingredients, index, acc + 1);
            }

        removeAtIndex(myIngredients, MenuManager.showOptions(myIngredients), 1);
    }

    def mainLoop(myIngredients: List[Ingredient]): Any = {
        val i = showMenu(List("Add Ingredients", "Remove Ingredients", "Show My Ingredients"));

        i match {
            case 0 => sys.exit();
            case _ => println("Error: Please provide a valid input!");
        }
    }

    def main(args: Array[String]): Unit = {
        mainLoop(List[Ingredient]());
    }
}