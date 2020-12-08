import java.io.{File, PrintWriter}

import MenuManager.{askForStringInput, showMenu}
import sun.security.util.Password

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source
/*
case class UserAuthentication(username: String, password: String, name: String);

object UserAuthentication {

  def create(username: String, password: String, name: String): UserAuthentication = new UserAuthentication(username, password, name);

  def getUsername(user: UserAuthentication): String = user.username;

  def findUser(users: List[UserAuthentication], username: String, password: String): Option[UserAuthentication] = {
    users match {
      case Nil => None;
      case user :: usersList => if((user.username == username) && (user.password == password)) { Some(user) }
      else { findUser(usersList, username, password) }
    }
  }

  def createAccount(users: List[UserAuthentication]): List[UserAuthentication] = {
    def createNewUsername(users: List[UserAuthentication]): String = {
      def usernameExists(users: List[UserAuthentication], username: String): Boolean = {
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

    UserAuthentication.create(createNewUsername(users), askForStringInput("Password: "), askForStringInput("Name: ")) :: users;
  }

  def accountLoop(users: List[UserAuthentication]): UserAuthentication = {
    val i = showMenu(List("Login", "Create Account"));

    i match {
      case 0 => sys.exit();
      case 1 => {
        findUser(users, askForStringInput("Username: "), askForStringInput("Password: ")) match {
          case None => println("Error: invalid credentials."); accountLoop(users);
          case Some(user) => user;
        }
      }
      case 2 => accountLoop(Database.saveUsers(createAccount(users)));
      case _ => { println("Error: Please provide a valid input!"); accountLoop(users); }
    }
  }

  def init(): UserAuthentication = {
    accountLoop(Database.loadUsers());
  }

}*/