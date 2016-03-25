package controllers

import javax.inject.Inject

import org.joda.time.DateTime
import models.{TodoRepo, Todo, TodoStatus}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{BodyParsers, Action, Controller}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import utils.EnumUtils


/**
 * Created by hluu on 3/18/16.
 */
class TodoController @Inject()( todoRepo: TodoRepo)
  extends Controller {

  def createTodo = Action(BodyParsers.parse.json) { implicit request =>
    val placeResult = request.body.validate[Todo]
    placeResult.fold(
      errors => {
        BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
      },
      todo => {

        val newId = todoRepo.create2(todo)
        Ok(Json.obj("status" ->"OK"))
        //Ok(Json.obj("status" ->"OK", "message" -> ("Todo  '"+todo.title+"' saved.") ))
      }
    )
  }

  def createTodo2(title: String)= Action.async { implicit rs =>
    todoRepo.create(title)
      .map(id => Ok(s"Todo $id created") )
  }

  def listTodos = Action.async { implicit rs =>
    todoRepo.all
      .map(todos => Ok(Json.toJson(todos)))
  }


  def todos(id: Long) = Action.async { implicit rs =>
    for {
      Some(todo) <-  todoRepo.findById(id)
    } yield Ok(Json.toJson(todo))
  }


  /*implicit val TodoWrites = new Writes[Todo] {
    def writes(todo: Todo) = Json.obj(
      "id" -> todo.id,
      "title" -> todo.title,
      "desc" -> todo.desc,
      "status" -> todo.status,
      "createdDate" -> todo.createdDate
    )
  } */

  implicit val myEnumReads: Reads[TodoStatus.Value] = EnumUtils.enumReads(TodoStatus)

  implicit val TodoWrites : Writes[Todo] = (

    (JsPath \ "id").write[Long] and
      (JsPath \ "title").write[String] and
      (JsPath \ "desc").write[String] and
      (JsPath \ "status").write[TodoStatus.Value] and
      (JsPath \ "createdDate").write[DateTime]
    ) (unlift(Todo.unapply))



  implicit  val TodoReads : Reads[Todo] = (
    (JsPath \ "id").read[Long] and
    (JsPath \ "title").read[String](maxLength[String](100)) and
      (JsPath \ "desc").read[String] and
      (JsPath \ "status").read[TodoStatus.Value] and
      (JsPath \ "createdDate").read[DateTime]

    ) (Todo.apply _)


}

