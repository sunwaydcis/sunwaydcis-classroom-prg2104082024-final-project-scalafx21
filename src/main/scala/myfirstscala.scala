//import statements
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.geometry.Insets
import scalafx.scene.control.Menu.sfxMenu2jfx
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import java.io.*
import java.time.LocalDate
import scala.collection.mutable
import scalafx.scene.input.KeyEvent
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scalafx.application.Platform

//Defining the case class named Task
// Task has three attributes:
// - name (String): the name of the task
// - description (String): a brief description of the task
// - isCompleted (Boolean): indicates whether the task is completed (default is false)

//Trait defining a common behaviour for display
trait Displayable {
  def displayInfo: String // Abstract method to provide a formatted string
}

// SubTask class to manage subtasks
case class SubTask(name: String, description: String, isCompleted: Boolean = false) {
  def displayInfo: String = {
    s" Subtask: $name - $description"
  }
}


abstract class Task(val name: String, val description: String, var isCompleted: Boolean = false) extends Displayable {  // Removed dueDate
  def taskType: String // Abstract method to define the task type

  // Updated displayInfo to exclude the dueDate
  override def displayInfo: String = {
    s" $taskType: $name - $description"
  }
}

class PersonalTask(name: String, description: String, isCompleted: Boolean = false)
  extends Task(name, description, isCompleted) {  // Removed dueDate
  override def taskType: String = "Personal"
}

class WorkTask(name: String, description: String, isCompleted: Boolean = false)
  extends Task(name, description, isCompleted) {  // Removed dueDate
  override def taskType: String = "Work"
}

case class SpecialTask(override val name: String, override val description: String)
  extends Task(name, description) {
  override def taskType: String = "Special"
}
//Defining the class Task Manager
//Task Manager is responsible for managing the tasks.
class TaskManager[T <: Task] {
  private val tasks: mutable.ListBuffer[T] = mutable.ListBuffer()

  def addTask(task: T): Unit = tasks += task // Adds tasks to the tasks list
  def removeTask(task: T): Unit = tasks -= task // Removes tasks from the tasks list
  def getTasks: List[T] = tasks.toList //Returns all tasks as an immutable list
  def markAsCompleted(task: T): Unit = {
    tasks.find(_ == task).foreach(_.isCompleted = true) // Marks the task as completed
  }
  def clearAll(): Unit = tasks.clear() // Clears all the tasks
  def getCompletedTasks: List[T] = tasks.filter(_.isCompleted).toList // Gets the completed tasks

  // Updated saveToFile method
  def saveToFile(filePath: String): Unit = {
    val file = new File(filePath)
    val writer = new PrintWriter(file)
    tasks.foreach { task =>
      writer.println(s"${task.name}|${task.description}|${task.isCompleted}")
    }
    writer.close()
  }
}

object TaskManagementApp extends JFXApp3 {

  override def start(): Unit = {
    val taskManager = new TaskManager[Task]()
    val taskList = ObservableBuffer[Task]()

    // Define dark mode styles
    val darkStyle =
      """
      .root {
        -fx-background-color: #2e2e2e;
      }
      .text-field, .text-area, .combo-box, .date-picker {
        -fx-background-color: #444444;
        -fx-text-fill: white;
        -fx-border-color: #666666;
      }
      .button {
        -fx-background-color: #4caf50;
        -fx-text-fill: white;
        -fx-border-radius: 5px;
        -fx-background-radius: 5px;
      }
      .button:hover {
        -fx-background-color: #388e3c;
      }
      .list-view {
        -fx-background-color: #444444;
        -fx-text-fill: white;
        -fx-border-color: #666666;
      }
      .menu-bar {
        -fx-background-color: #333333;
        -fx-text-fill: white;
      }
      .menu-item {
        -fx-background-color: #333333;
        -fx-text-fill: white;
      }
      .menu-item:hover {
        -fx-background-color: #555555;
      }
    """

    val taskNameField = new TextField {
      promptText = "Task Name"
      style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-background-color: white; -fx-border-color: #cccccc; -fx-border-radius: 5px; -fx-padding: 10px;"
    }

    val taskDescriptionField = new TextArea {
      promptText = "Task Description"
      wrapText = true
      prefHeight = 60
      style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-background-color: white; -fx-border-color: #cccccc; -fx-border-radius: 5px; -fx-padding: 10px;"
    }

    val taskTypeComboBox = new ComboBox[String](Seq("Personal", "Work","Special")) {
      promptText = "Select Task Type"
      style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-background-color: white; -fx-border-color: #cccccc; -fx-border-radius: 5px; -fx-padding: 5px;"

    }

    val addButton = new Button("Add Task") {
      onAction = _ => {
        val name = taskNameField.text()
        val description = taskDescriptionField.text()
        val taskType = taskTypeComboBox.value()

        if (name.nonEmpty && taskType != null) {
          val task = taskType match {
            case "Personal" => new PersonalTask(name, description)
            case "Work" => new WorkTask(name, description)
            case "Special" => SpecialTask(name, description)
          }
          taskManager.addTask(task)
          taskList += task


          // Clear input fields
          taskNameField.clear()
          taskDescriptionField.clear()
          taskTypeComboBox.value = null

          // No need to clear dueDateField as it's no longer used
        } else {
          new Alert(Alert.AlertType.Warning) {
            title = "Input Error"
            headerText = "Missing Fields"
            contentText = "Please ensure all required fields are filled."
          }.showAndWait()
        }
      }
      style = "-fx-font-size: 16px; -fx-font-weight: bold; -fx-background-color: #4caf50; -fx-border-radius: 5px; -fx-background-radius: 5px; -fx-padding: 10px; -fx-text-fill: white; -fx-cursor: hand;"
    }

    // Task list view
    val taskListView = new ListView[Task](taskList) {
      cellFactory = (_: ListView[Task]) => new ListCell[Task] {
        item.onChange { (_, _, task) =>
          text = if (task != null) {
            val status = if (task.isCompleted) "[Completed]" else "[Pending]"
            val taskType = task match {
              case _: PersonalTask => "Personal"
              case _: WorkTask => "Work"
              case special: SpecialTask => "Special"
              case _ => "Unknown"
            }
            // Removed dueDate handling since it's no longer part of the Task class
            s"${task.name} - $taskType"
          } else ""
        }
      }
      style = "-fx-background-color: white; -fx-border-color: #f4f4f9; -fx-border-radius: 5px; -fx-padding: 10px;"
    }

    val removeButton = new Button("Remove Selected") {
      onAction = _ => {
        val selectedTask = taskListView.selectionModel().getSelectedItem
        if (selectedTask != null) {
          taskManager.removeTask(selectedTask)
          taskList -= selectedTask
        }
      }
      style = "-fx-font-size: 16px; -fx-font-weight: bold; -fx-background-color: #4caf50; -fx-border-radius: 5px; -fx-background-radius: 5px; -fx-padding: 10px; -fx-text-fill: white; -fx-cursor: hand;"
    }

    val markCompletedButton = new Button("Mark as Completed") {
      onAction = _ => {
        val selectedTask = taskListView.selectionModel().getSelectedItem
        if (selectedTask != null && !selectedTask.isCompleted) {
          taskManager.markAsCompleted(selectedTask)
          taskList.update(taskList.indexOf(selectedTask), selectedTask)
        }
      }
      style = "-fx-font-size: 16px; -fx-font-weight: bold; -fx-background-color: #4caf50; -fx-border-radius: 5px; -fx-background-radius: 5px; -fx-padding: 10px; -fx-text-fill: white; -fx-cursor: hand;"
    }

    // Menu bar setup
    val menuBar = new MenuBar {
      menus = Seq(
        new Menu("File") {
          items = Seq(
            new MenuItem("Exit") {
              onAction = _ => sys.exit(0)
            }
          )
        },
        new Menu("View") {
          items = Seq(
            new MenuItem("View Completed Tasks") {
              onAction = _ => {
                val completedTasks = taskManager.getCompletedTasks
                taskList.clear()
                taskList ++= completedTasks
              }
            },
            new MenuItem("View All Tasks") {
              onAction = _ => {
                taskList.clear()
                taskList ++= taskManager.getTasks
              }
            }
          )
        },
        new Menu("Help") {
          items = Seq(
            new MenuItem("About") {
              onAction = _ => println("Task Management System v1.0")
            }
          )
        }
      )
    }
    // Layout
    val inputPane = new VBox(10) {
      padding = Insets(10)
      children = Seq(taskNameField, taskDescriptionField, taskTypeComboBox, addButton)
    }

    val controlPane = new HBox(10) {
      padding = Insets(10)
      children = Seq(markCompletedButton, removeButton)
    }

    val rootPane = new BorderPane {
      top = new VBox(menuBar, new Label("Task Management System") {
        style = "-fx-font-size: 20px; -fx-font-weight: bold; -fx-padding: 10;"
      })
      center = new VBox(10, new Label("Task List"), taskListView, controlPane)
      left = inputPane
    }

    stage = new JFXApp3.PrimaryStage {
      title = "Task Management System"
      scene = new Scene {
        root = rootPane
        stylesheets.add(darkStyle)
      }
    }
  }
}


  
