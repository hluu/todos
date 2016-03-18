package controllers

import javax.inject.Inject

import models.{TodoRepo}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Action, Controller}

/**
 * Created by hluu on 3/18/16.
 */
class TodoController @Inject()( todoRepo: TodoRepo)
  extends Controller {

  def createTodo(title: String)= Action.async { implicit rs =>
    todoRepo.create(title)
      .map(id => Ok(s"Todo $id created") )
  }

  def listTodos = Action.async { implicit rs =>
    todoRepo.all
      .map(todos => Ok(views.html.todos(todos)))
  }

  def todos(id: Long) = Action.async { implicit rs =>
    for {
      Some(todo) <-  todoRepo.findById(id)
    } yield Ok(views.html.todo(todo))
  }
}
