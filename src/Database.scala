import java.io._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import Ingredient.dbIngredients
import MenuManager.{askForStringInput, showMenu}

case class Database(users: List[User], recipes: List[Recipe], ingredients: List[Ingredient]) extends Serializable;

case class objectTest(ings: List[Ingredient]) extends Serializable;

object Database {
/*
  def loadUserCredentials(): List[UserAuthentication] = {
    val bufferedSource = Source.fromFile("users.txt");
    var usersBuffer = new ListBuffer[UserAuthentication]();

    for (line <- bufferedSource.getLines){
      val userData = line.split(";");
      val username = userData(0);
      val password = userData(1);
      val name = userData(2);
      usersBuffer += UserAuthentication.create(username, password, name);
    }

    bufferedSource.close;
    usersBuffer.toList;
  }

  def saveUserCredentials(registeredUsers: List[UserAuthentication]): List[UserAuthentication] = {
    def saveUsersToFile(printer: PrintWriter, users: List[UserAuthentication]): Unit = {
      users match {
        case Nil => Nil;
        case user :: usersList => printer.write(user.username + ";" + user.password + ";" + user.name + "\n"); saveUsersToFile(printer, usersList);
      }
    }

    val pw = new PrintWriter(new File("users.txt"))

    saveUsersToFile(pw, registeredUsers);

    pw.close;

    registeredUsers;
  }
*/
  def loadObjects(filename: String): Any = {
    try {
      val file = new FileInputStream(filename);
      val input = new ObjectInputStream(file);
      input.readObject().asInstanceOf[Any];
    } catch {
      case _:Throwable => None;
    }
  }

  def saveObjects(objects: Any, filename: String): Unit = {
    val file = new FileOutputStream(filename);
    val output = new ObjectOutputStream(file);
    output.writeObject(objects);
    output.close();
    file.close();
  }

  def usernameExists(username: String, users: List[User]): Boolean = {
    users match {
      case Nil => false;
      case user :: usersList => if (username == user.username) true else usernameExists(username, usersList);
    }
  }

  def updateUser(username: String, f: User => User, database: Database): Database = {
    def updateUserList(username: String,  f: User => User, users: List[User]): List[User] = {
     users match {
       case Nil => Nil;
       case u :: ul => if(u.username == username) f(u) :: ul else u :: updateUserList(username, f, ul);
     }
    }
    new Database(updateUserList(username, f, database.users), database.recipes, database.ingredients);
  }

  def getUser(username: String, users: List[User]): Option[User] = {
    users match {
      case Nil => None;
      case u :: ul => if(u.username == username) Some(u) else getUser(username, ul);
    }
  }

  def createAccount(username: String, password: String, name: String, database: Database): Database = {
    new Database(User.create(username, password, name) :: database.users, database.recipes, database.ingredients);
  }

  /*
  def createAccount(users: List[User]): List[User] = {
    def createNewUsername(users: List[User]): String = {
      def usernameExists(users: List[User], username: String): Boolean = {
        users match {
          case Nil => false;
          case user :: usersList => if (username == user.username) true else usernameExists(usersList, username);
        }
      }
      val newUsername = askForStringInput("Username: ");
      if( usernameExists(users, newUsername) ) {
        println("Username exists!");
        createNewUsername(users);
      }
      else newUsername;
    }

    User.create(createNewUsername(users), askForStringInput("Password: "), askForStringInput("Name: ")) :: users;
  }*/

  def main(args: Array[String]): Unit = {
    val test = loadObjects("test.db").asInstanceOf[objectTest];
    test.ings.foreach(ing => println(ing._name));
  }
}