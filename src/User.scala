import MenuManager._

import scala.annotation.tailrec
import scala.io.Source
import java.io._

import scala.collection.mutable.ListBuffer

case class User(username: String, password: String, name: String);

object User {
  def dbRegisteredUsers: List[User] = List(
    User("rubenfonte", "ppmapp", "Ruben"),
    User("danielborges", "ppmapp", "Daniel"),
    User("joaofernandes", "ppmapp", "Joao")
  )

  def loadUsers(file: String): List[User] = {
    val bufferedSource = Source.fromFile(file);
    var usersBuffer = new ListBuffer[User]();

    for (line <- bufferedSource.getLines){
        val userData = line.split(";");
        val username = userData(0);
        val password = userData(1);
        val name = userData(2);
        usersBuffer += new User(username, password, name);
    }

    bufferedSource.close;
    usersBuffer.toList;
  }

  def saveUsers(file: String, registeredUsers: List[User]): List[User] = {
    def saveUsersToFile(printer: PrintWriter, users: List[User]) = {
      users.foreach(user =>
        printer.write(user.username + ";" + user.password + ";" + user.name + "\n"));
    }
    val pw = new PrintWriter(new File(file))

    saveUsersToFile(pw, registeredUsers);

    pw.close;

    registeredUsers;
  }

  @tailrec
  def login(users: List[User]): User = {
    val username = askForStringInput("Username: ");
    val password = askForStringInput("Password: ");
    val foundUser = users.find(user => (user.username == username) && (user.password == password));

    foundUser match {
      case Some(user) => user;
      case None => { println("Error: invalid credentials."); login(users); }
    }

  }

  @tailrec
  def createAccount(users: List[User]): List[User] = {
    val username = askForStringInput("Username: ");
    val userExists = users.exists(user => user.username == username);

    if(userExists) {
      println("Username exists!");
      createAccount(users);
    }
    else {
      val password = askForStringInput("Password: ");
      val name = askForStringInput("Name: ");
      val newUser = User(username,password,name);
      newUser :: users;
    }

  }

  def accountLoop(users: List[User]): User = {
    val i = showMenu(List("Login", "Create Account"));

    i match {
      case 0 => sys.exit();
      case 1 => login(users);
      case 2 => accountLoop(saveUsers("users.txt", createAccount(users)));
      case _ => { println("Error: Please provide a valid input!"); accountLoop(users); }
    }
  }

  def main(args: Array[String]): Unit = {
    val dbUsers = loadUsers("users.txt");

    val user = accountLoop(dbUsers);
    println(user.name);
  }

}
