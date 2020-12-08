import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Paths}

object DatabaseManager {

  def databaseFilePath = "database.db";

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

  def dbRecipes: List[Recipe] = List(
    new Recipe("Bacalhau à Brás", List(new Ingredient("Bacalhau", "Fish", 100), new Ingredient("Batata", "Vegetables", 20), new Ingredient("Cebola", "Vegetables", 14)), List("Pescetarianismo","Carnivora"), "easy",List(""),0),
    new Recipe("Strogonoff de Frango", List(new Ingredient("Frango", "Meat", 150), new Ingredient("Natas", "Lacticinios", 150), new Ingredient("Cebola", "Vegetables", 12)), List("Pescetarianismo","Carnivora"), "medium",List(""),0),
    new Recipe("Arroz de Pato", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Pato", "Lacticinios", 234), new Ingredient("Cebola", "Vegetables",12)), List("Vegetariano","Carnivora"), "hard",List(""),0),
    new Recipe("Arroz de Frango", List(new Ingredient("Arroz", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Massa de Frango", List(new Ingredient("Massa", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
    new Recipe("Sopa de Frango", List(new Ingredient("Agua", "Cereal", 80), new Ingredient("Frango", "Meats", 150), new Ingredient("Cebola", "Vegetables",12)), List("Vegan"), "easy",List(""),0),
  )

  def loadDatabase(): Option[Database] = {
    try {
      if(Files.exists(Paths.get("database.db")))
      {
        val file = new FileInputStream("database.db");
        val input = new ObjectInputStream(file);
        val database = input.readObject().asInstanceOf[Option[Database]];
        database;
      }
      else {
        Option(new Database(List(), dbRecipes, dbIngredients));
      }
    } catch {
      case _:Throwable => None;
    }
  }

  def saveDatabase(database: Database): Unit = {
    val file = new FileOutputStream("database.db");
    val output = new ObjectOutputStream(file);
    output.writeObject(database);
  }

  def loadObjects(): Any = {
    try {
      val file = new FileInputStream(databaseFilePath);
      val input = new ObjectInputStream(file);
      input.readObject().asInstanceOf[Any];
    } catch {
      case _:Throwable => None;
    }
  }

  def saveObjects(objects: Any): Unit = {
    val file = new FileOutputStream(databaseFilePath);
    val output = new ObjectOutputStream(file);
    output.writeObject(objects);
    output.close();
    file.close();
  }
}
