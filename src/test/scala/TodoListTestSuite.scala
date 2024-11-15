import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ListBuffer

class TodoListTestSuite extends AnyFlatSpec {


  "The todo list" should "have size 0" in {
    assert(TodoList.todoList.isEmpty)
  }

  "Adding a task" should "not fail" in {
    val newTaskAdded = TodoList.addTask("task 1", 90)
    assert(newTaskAdded.getName == "task 1")
    assert(newTaskAdded.getPriority == 90)
  }

  "Todo list" should "have one element" in {
    assert(TodoList.todoList.length == 1)
  }

//  "Saving a todo list in a file" should "succeed" in {
//    TodoList.saveListToFile("one element list")
//  }
//
//  "Loading a file" should "flush and replace the current task list" in {
//    TodoList.addTask("task 100", 90)
//    TodoList.loadFileToList("one element list")
//    assert(TodoList.todoList.length == 1)
//    assert(TodoList.todoList.last.getName == "task 1")
//  }

  "Printing the current directory" should "display all files" in {
    val list = TodoList.getFileNames("")
    assert(list.nonEmpty)
  }

  "Finish a task" should "flag the task as finished and have a finished date defined" in {
    val selectedTask = TodoList.finishTask(1)
    TodoList.todoList.find(x => x.getId == selectedTask.getId) match
      case Some(task) =>
        assert(task.getIsFinished)
        assert(task.getFinishedDate.nonEmpty)
      case _ => fail()

  }

  "Modifying a task" should "allow you to change name and priority" in {
    val firstTask = TodoList.todoList.head
    TodoList.modifyTask("renamed task", 50, firstTask.getId )
    assert(firstTask.getName == "renamed task")
    assert(firstTask.getPriority == 50)
  }

  "Created date" should "be initialized" in {
    val firstTaskDate = TodoList.todoList.head.getCreationDate
  }

  "Deleting a task" should "Succeed" in {
    val firstTask = TodoList.todoList.head
    val id = firstTask.getId
    TodoList.deleteTask(id)
    TodoList.todoList.find(x => x.getId == id) match
      case Some(task) => fail()
      case None => succeed
  }




}
