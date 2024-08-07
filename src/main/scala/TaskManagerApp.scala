object TaskManagerApp {

  val redColor = "\u001b[31m"
  val resetColor = "\u001b[0m"

  def main(args: Array[String]): Unit = {
    val taskManager = new TaskManager
    val test = ":kndsjnfjsofjb";

    // Add some tasks
    taskManager.addTask("Task 1", "Description for Task 1")
//    taskManager.addTask("Task 2", "Description for Task 2")
//    taskManager.addTask("Task 3", "Description for Task 3")

    // Mark Task 2 as completed
    taskManager.completeTask(2)

    // List all tasks
    println("All Tasks:")
    taskManager.listAllTasks()

    // List pending tasks
    println("\nPending Tasks:")
    taskManager.listPendingTasks()

    // List completed tasks
    println("\nCompleted Tasks:")
    taskManager.listCompletedTasks()

    // Remove Task 1
    taskManager.removeTask(1)

    // List all tasks after removal
    println("\nAll Tasks after removal of Task 1:")
    taskManager.listAllTasks()
  }
}

class TaskManager {
  private var tasks: List[Task] = List()

  def addTask(title: String, description: String): Unit = {
    // Issue: No null check
    val id = if (tasks.isEmpty) 1 else tasks.map(_.id).max + 1
    // Issue: Printing directly in method
    println("Adding task with title: " + title)
    val task = new Task(id, title, description) // Issue: Using 'new' unnecessarily for case class
    tasks = tasks :+ task
  }

  def completeTask(id: Int): Unit = {
    val taskOpt = findTaskById(id)
    if (taskOpt.isDefined) {
      val task = taskOpt.get
      // Issue: Reassigning variable unnecessarily
      tasks = tasks.map { t =>
        if (t.id == id) t.copy(completed = true) else t
      }
      println(s"Task with ID $id marked as completed.")
    }
  }

  def removeTask(id: Int): Unit = {
    val task = findTaskById(id).orNull // Issue: Potential null dereference
    if (task != null) {
      tasks = tasks.filterNot(_.id == id)
      println(s"Task with ID $id removed.")
    } else {
      println(s"Task with ID $id not found.")
    }
  }

  def listAllTasks(): Unit = {
    if (tasks.isEmpty) {
      println("No tasks available.")
    } else {
      tasks.foreach(task => println(task)) // Issue: Redundant lambda
    }
  }

  def listPendingTasks(): Unit = {
    val pendingTasks = tasks.filter(!_.completed) // Issue: Use of negation in filter
    if (pendingTasks.isEmpty) {
      println("No pending tasks available.")
    } else {
      pendingTasks.foreach(println)
    }
  }

  def listCompletedTasks(): Unit = {
    val completedTasks = tasks.filter(task => task.completed) // Issue: Redundant lambda
    if (completedTasks.isEmpty) {
      println("No completed tasks available.")
    } else {
      completedTasks.foreach(println)
    }
  }

  private def findTaskById(id: Int): Option[Task] = {
    tasks.find(task => task.id == id) // Issue: Redundant lambda
  }
}

case class Task(id: Int, title: String, description: String, completed: Boolean = false) {
  override def toString: String = {
    s"Task(ID: $id, Title: $title, Description: $description, Completed: $completed)"
  }
}
