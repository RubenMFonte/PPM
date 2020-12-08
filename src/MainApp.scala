import java.nio.file.{Files, Paths}

import DatabaseManager.{dbIngredients, dbRecipes}
import MenuManager._

object MainApp {
  def databaseFilePath: String = "database.db";
  def mainOptions: List[String] = List("Login", "Create Account", "Exit");
  def accountOptions: List[String] = List("Suggestions", "My Ingredients", "Exit");
  def ingredientsOptions: List[String] = List("View My Ingredients", "Add Ingredient", "Remove Ingredient", "Back");

  def accountLoop(activeUser: String, database: Database): Database = {
    showMenu(accountOptions) match {
      case 0 => database;
      case 1 => MenuManager.printStringsOnScreen(Recipe.getRecipesNamesAsList(Recipe.suggestRecipes(database.recipes))); accountLoop(activeUser, database);
      case 2 => myIngredientsLoop(activeUser, database);
    }
  }


  def myIngredientsLoop(activeUser: String, database: Database): Database = {

    showMenu(ingredientsOptions) match {
      case 1 => {
        Database.getUser(activeUser, database.users) match {
          case Some(user) => MenuManager.printStringsOnScreen(Ingredient.getIngredientsNamesAsList(Ingredient.getMyIngredients(user.myIngredients, database.ingredients))); myIngredientsLoop(activeUser, database);
          case None => myIngredientsLoop(activeUser, database);
        }
      }
      case 2 => myIngredientsLoop(activeUser, Database.updateUser(activeUser, User.addIngredient, database));
      case 3 => myIngredientsLoop(activeUser, Database.updateUser(activeUser, User.removeIngredient, database));
      case 4 => database;
    }
  }

  def mainLoop(database: Database): Database = {
    showMenu(mainOptions) match {
      case 0 => database;
      case 1 => {
        User.loginUser(database.users, askForStringInput("Username: "), askForStringInput("Password: ")) match {
          case None => println("Error: invalid credentials."); mainLoop(database);
          case Some(username) => accountLoop(username, database);
        }
      }
      case 2 => {
        val username = askForStringInput("Username: ");
        if(!Database.usernameExists(username, database.users)) {
          mainLoop(Database.createAccount(username, askForStringInput("Password: "), askForStringInput("Name: "), database));
        }
        else {
          println("Username already exists!");
          mainLoop(database);
        }
      }
      case _ => { println("Error: Please provide a valid input!"); mainLoop(database); }
    }
  }

  def main(args: Array[String]): Unit = {
    if(Files.exists(Paths.get(databaseFilePath))) {
      DatabaseManager.saveObjects(mainLoop(DatabaseManager.loadObjects().asInstanceOf[Database]))
    };
    else {
      DatabaseManager.saveObjects(mainLoop(new Database(List(), dbRecipes, dbIngredients)));
    }
  }
}
