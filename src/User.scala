
case class User(username: String, password: String, name: String, favRecipes: List[String], myMenus: List[MealMenu], myIngredients: List[String]) extends Serializable;

object User {

  def create(username: String, password: String, name: String): User = new User(username, password, name, List(), List(), List());

  def loginUser(users: List[User], username: String, password: String): Option[String] = {
    users match {
      case Nil => None;
      case user :: usersList => if((user.username == username) && (user.password == password)) { Some(user.username) }
      else { loginUser(usersList, username, password) }
    }
  }

  def getUser(username: String, users: List[User]): Option[User] = {
    users match {
      case Nil => None;
      case u :: ul => if(u.username == username) Option(u) else getUser(username, ul);
    }
  }

  def addIngredient(user: User): User = {
    new User(user.username, user.password, user.name, user.favRecipes, user.myMenus, Ingredient.addIngredient(user.myIngredients));
  }

  def removeIngredient(user: User): User = {
    new User(user.username, user.password, user.name, user.favRecipes, user.myMenus, Ingredient.removeIngredient(user.myIngredients));
  }
}