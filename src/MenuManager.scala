import scala.io.StdIn.readLine;

object MenuManager {

  def showMenu(options: List[String]): Int = {
    def showOptions(options: List[String], acc: Int): String = {
      options match {
        case Nil => "0) Exit";
        case s :: ops => acc + ") " + s + ", " + showOptions(ops, acc + 1);
      }
    }

    println(showOptions(options, 1));
    readLine().toInt;
  }

  def askForStringInput(question: String): String = {
    println(question);
    readLine();
  }
}
