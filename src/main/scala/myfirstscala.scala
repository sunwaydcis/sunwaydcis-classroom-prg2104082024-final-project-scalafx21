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
case class SubTask(name: String, description: String, isCompleted: Boolean = false,val priority: Int = 0) {
  def displayInfo: String = {
    val status = if (isCompleted) "[Completed]" else "[Pending]"
    s"$status Subtask: $name - $description"
  }
}


abstract class Task(val name: String, val description: String, var isCompleted: Boolean = false,val dueDate: Option[LocalDate] = None,val priority: Int = 0) extends Displayable {
  def taskType: String // Abstract method to define the task type
  def subTasks: List[SubTask] = List() // Subtasks related to the task

  // Proper implementation of displayInfo
  override def displayInfo: String = {
    val status = if (isCompleted) "[Completed]" else "[Pending]"
    val due = dueDate.map(d => s"Due: $d").getOrElse("No due date")
    val priorityInfo = if (priority > 0) s"Priority: $priority" else "No priority"
    s"$status $taskType: $name - $description- $due - $priorityInfo"
  }
}

class PersonalTask(name: String, description: String, dueDate: Option[LocalDate] = None,isCompleted: Boolean = false)
  extends Task(name, description, isCompleted,dueDate) {
  override def taskType: String = "Personal"
}

class WorkTask(name: String, description: String,dueDate: Option[LocalDate] = None,isCompleted: Boolean = false)
  extends Task(name, description, isCompleted,dueDate) {
  override def taskType: String = "Work"
}

case class SpecialTask(override val name: String, override val description: String, override val priority: Int, override val dueDate: Option[LocalDate])
  extends Task(name, description, priority = priority, dueDate = dueDate) {
  override def taskType: String = s"Special (Priority $priority)"
}

//Defining the class Task Manager
//Task Manager is responsible for managing the tasks.
class TaskManager[T <: Task] {
  private val tasks: mutable.ListBuffer[T] = mutable.ListBuffer()

  def addTask(task:T): Unit = tasks += task //adds tasks to the tasks list .
  def removeTask(task:T): Unit = tasks -= task //remove tasks from the tasks list.
  def getTasks: List[T] = tasks.sortBy(_.dueDate.getOrElse(LocalDate.MAX)).toList //get the tasks list
  def markAsCompleted(task:T): Unit = {
    tasks.find(_ == task).foreach(_.isCompleted = true) //marks the completed tasks
  }
  def clearAll(): Unit = tasks.clear()//clears all the tasks
  def getCompletedTasks: List[T] = tasks.filter(_.isCompleted).toList // gets the completed tasks

  def saveToFile(filePath: String): Unit = {
    val file = new File(filePath)
    val writer = new PrintWriter(file)
    tasks.foreach { task =>
      writer.println(s"${task.name}|${task.description}|${task.isCompleted}|${task.dueDate.getOrElse("")}|${task.priority}")
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

    val priorityField = new TextField {
      promptText = "Priority (for Special Tasks)"
      disable = true
      style = "-fx-font-size: 14px; -fx-font-weight: bold; -fx-background-color: white; -fx-border-color: #cccccc; -fx-border-radius: 5px; -fx-padding: 10px;"
    }

    val dueDateField = new DatePicker {
      promptText = "Select Due Date"
    }

    // Enable priority input only for SpecialTask
    taskTypeComboBox.onAction = _ => {
      priorityField.disable = taskTypeComboBox.value() != "Special"
    }

    val addButton = new Button("Add Task") {
      onAction = _ => {
        val name = taskNameField.text()
        val description = taskDescriptionField.text()
        val taskType = taskTypeComboBox.value()
        val dueDate = Option(dueDateField.value.value)
        val priority = priorityField.text().toIntOption.getOrElse(0)

        if (name.nonEmpty && taskType != null) {
          val task = taskType match{
            case "Personal" => new PersonalTask(name, description)
            case "Work" => new WorkTask(name, description)
            case "Special" => SpecialTask(name, description, priority,dueDate)
          }
          taskManager.addTask(task)
          taskList += task

          // Clear input fields
          taskNameField.clear()
          taskDescriptionField.clear()
          priorityField.clear()
          taskTypeComboBox.value = null
          dueDateField.value = null
        }else{
          new Alert(Alert.AlertType.Warning) {
            title = "Input Error"
            headerText = "Missing Fields"
            contentText = "Please ensure all required fields are filled."
          }.showAndWait()
        }
      }
      style = "-fx-font-size: 16px; -fx-font-weight: bold; -fx-background-color: #4caf50; -fx-border-radius: 5px; -fx-background-radius: 5px; -fx-padding: 10px; -fx-text-fill: white; -fx-cursor: hand;"
    }

    //Task list view
    val taskListView = new ListView[Task](taskList) {
      cellFactory = (_: ListView[Task]) => new ListCell[Task] {
        item.onChange { (_, _, task) =>
          text = if (task != null) {
            val status = if (task.isCompleted) "[Completed]" else "[Pending]"
            val taskType = task match{
              case _: PersonalTask => "Personal"
              case _: WorkTask     => "Work"
              case special: SpecialTask => s"Special (Priority: ${special.priority})"
              case _               => "Unknown"
            }
            val dueDate = task match {
              case special: SpecialTask => special.dueDate.map(_.toString).getOrElse("No Due Date")
              case personal: PersonalTask => personal.dueDate.map(_.toString).getOrElse("No Due Date")
              case work: WorkTask => work.dueDate.map(_.toString).getOrElse("No Due Date")
              case _ => "N/A"
            }
            s"$status ${task.name} - $taskType | Due: $dueDate"
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
      children = Seq(taskNameField, taskDescriptionField, taskTypeComboBox, priorityField, dueDateField, addButton)
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

 

