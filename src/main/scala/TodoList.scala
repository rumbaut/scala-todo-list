import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*
/*
* Create a menu
Add a task
Print all Tasks
Modify a task
Complete a Task
Delete a Task
Save the task list
Load a task list

* */
object TodoList {
  val todoList : ListBuffer[Task]  = ListBuffer.empty

  class Task(private var id:Int,private var name: String,private var finished: Boolean,private var createdDate: LocalDateTime,private var finishedDate: Option[LocalDateTime],private var priority: Int) extends Serializable{
    def getId: Int = this.id
    def getName: String = this.name
    def getIsFinished: Boolean = this.finished
    def getCreationDate: LocalDateTime = this.createdDate
    def getFinishedDate: Option[LocalDateTime] = this.finishedDate
    def getPriority: Int = this.priority


    def setFinished(): Task ={
      if(!this.finished){
        this.finished = true
        this.finishedDate = Some(LocalDateTime.now())
      }
      this
    }

    def setName(name: String): Task ={
      this.name = name
      this
    }

    @throws[IllegalArgumentException]("If priority is equal or less than 0")
    def setPriority(priority: Int): Task = {
      if(priority <=0 ) throw new IllegalArgumentException("Priority cannot be less or equal to 0")
      this.priority = priority
      this
    }

  }

  enum MenuActions{
    case NewTask, FinishTask, DeleteTask, ModifyTask, SaveList, LoadList, PrintAllTask, ExitApp
  }

  def printMenu(): Unit = {
    println("1 - Add a new Task")
    println("2 - Complete a Task")
    println("3 - Delete a task")
    println("4 - Modify a task")
    println("5 - Save list on file")
    println("6 - Load List from file")
    println("7 - Print all Tasks")
    println("8 - Exit app")
  }

  /**
   *
   * @param name The task name
   * @param priority The priority of the task
   */
  def addTask(name: String, priority: Int): Task = {
    val id = if(todoList.isEmpty) 1 else todoList.length + 1
    todoList += Task(id, name, false, LocalDateTime.now(), None, priority)
    todoList.last
  }

  /**
   *
   * @param id Task id to search
   * @throws IllegalArgumentException If it does not find the task by the provided id
   */
  def finishTask(id: Int): Unit = {
    todoList.find(x => x.getId == id) match
      case Some(task) => task.setFinished()
      case _ => throw new IllegalArgumentException(s"Unable to find task with id: ${id}")
  }

  /**
   *
   * @param id task id to delete
   * @throws IllegalArgumentException If it does not find the task by id
   */
  def deleteTask(id: Int): Unit = {
    todoList.find(x => x.getId == id) match
      case Some(task) => todoList -= task
      case _ => throw new IllegalArgumentException(s"Unable to find task with id: ${id}")
  }

  /**
   *
   * @param name replaces the current name of the task
   * @param priority replaces the current priority of the task
   * @param id id to lookup for the task
   * @throws IllegalArgumentException If it does not find the task by the provided Id
   */
  def modifyTask(name: String, priority: Int, id: Int): Unit = {
    todoList.find(x => x.getId == id) match
      case Some(task) =>
        task.setName(name).setPriority(priority)
      case _ => throw new IllegalArgumentException(s"Unable to find task with id: ${id}")
  }


  private val fileExtension: String = ".sc.todo"

  /**
   *
   * @param filename Name of the file to overwrite or the on to create
   * @throws IOException if an I/O error occurs
   */
  def saveListToFile(filename: String): Unit = {
    val fileOut = FileOutputStream(filename + fileExtension)
    val out = new ObjectOutputStream(fileOut)
    try
      out.writeObject(todoList)
    finally
      out.close()
      fileOut.close()
  }

  def loadFileToList(filename: String): Unit = {
    val fileIn = new FileInputStream(filename + fileExtension)
    val in = new ObjectInputStream(fileIn)
    try
      val loadedList = in.readObject().asInstanceOf[ListBuffer[Task]]
      todoList.clear()
      todoList ++= loadedList
    finally
      in.close()
      fileIn.close()
  }

  def getFileNames(directory: String): List[String] =
    val dirPath = Paths.get(directory)
    if (Files.exists(dirPath) && Files.isDirectory(dirPath))
      Files.list(dirPath).iterator().asScala
        .filter(Files.isRegularFile(_))
        .map(_.getFileName.toString)
        .toList
    else
      List.empty[String]


  def main(args: Array[String]): Unit = {


  }
}
