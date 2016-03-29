package controllers

import javax.inject.Inject

import akka.actor.Status.{Failure, Success}
import org.joda.time.DateTime
import models.{TodoRepo, Todo, TodoStatus}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{BodyParsers, Action, Controller}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import utils.EnumUtils

import scala.concurrent.Future


/**
 * Created by hluu on 3/18/16.
 */
class TodoController @Inject()( todoRepo: TodoRepo)
  extends Controller {

  def createTodo = Action.async(BodyParsers.parse.json) { implicit request =>

   val placeResult = request.body.validate[Todo]

    placeResult match {
      case s: JsSuccess[Todo] =>  {
        val newId = todoRepo.create(s.get)
        Future(Ok(Json.obj("status" ->"OK")))
      }
      case e: JsError => Future(BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(e).toString())))
    }

  }

  def updateTodo =  Action.async(BodyParsers.parse.json) { implicit request =>
    val placeResult = request.body.validate[Todo]

    placeResult match {
      case s: JsSuccess[Todo] =>  {
        s.get.id match {
          case Some(id) => {
            val incomingTodo = s.get
            println("*** Update title: "+ incomingTodo.title)
            println(s"updateTodo: '$incomingTodo.id.get' $incomingTodo.title, $incomingTodo.desc")
            val updatedTodo = todoRepo.update(incomingTodo)
            Future(Ok(Json.obj("status" ->"OK")))
          }
          case None => {
            Future(BadRequest(Json.obj("status" ->"KO", "message" -> "missing id")))
          }
        }

      }
      case e: JsError => Future(BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toJson(e).toString())))
    }
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
      (JsPath \ "desc").read[String](maxLength[String](512)) and
      (JsPath \ "status").readNullable[TodoStatus.Value] and
      (JsPath \ "createdDate").readNullable[DateTime]

    ) (Todo.apply _)


}

