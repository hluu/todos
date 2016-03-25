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

    placeResult match {
      case s: JsSuccess[Todo] =>  {
        val newId = todoRepo.create2(s.get)
        Ok(Json.obj("status" ->"OK"))
      }
      case e: JsError => BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(e).toString()))
    }
    /*
    placeResult.fold(
      errors => {
        BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(errors)))
      },
      todo => {

        val newId = todoRepo.create2(todo)
        Ok(Json.obj("status" ->"OK"))
        //Ok(Json.obj("status" ->"OK", "message" -> ("Todo  '"+todo.title+"' saved.") ))
      }
    )    */
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
    val result = todoRepo.findById(id)
    result.map(r => {
      r match {
        case Some(todo) =>  Ok(Json.toJson(todo))
        case None => BadRequest(Json.obj("msg: " -> s"$id not found " ))
      }
    })

    /*
    for {
      Some(todo) <-  todoRepo.findById(id)
    } yield Ok(Json.toJson(todo))
    */
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

    (JsPath \ "id").writeNullable[Long] and
      (JsPath \ "title").write[String] and
      (JsPath \ "desc").write[String] and
      (JsPath \ "status").writeNullable[TodoStatus.Value] and
      (JsPath \ "createdDate").writeNullable[DateTime]
    ) (unlift(Todo.unapply))



  implicit  val TodoReads : Reads[Todo] = (
    (JsPath \ "id").readNullable[Long] and
    (JsPath \ "title").read[String](maxLength[String](100)) and
      (JsPath \ "desc").read[String] and
      (JsPath \ "status").readNullable[TodoStatus.Value] and
      (JsPath \ "createdDate").readNullable[DateTime]

    ) (Todo.apply _)


}

