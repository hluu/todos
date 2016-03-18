package models

import javax.inject.Inject
import org.joda.time.DateTime
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import scala.concurrent.Future
import SlickMapping.jodaDateTimeMapping

object TodoStatus extends Enumeration {
  val open = Value("open")
  val completed = Value("completed")
  val in_progress = Value("in-progress")
}


case class Todo(id: Long, title: String, desc: String, status: TodoStatus.Value, createdDate:DateTime) {
  def patch(title: Option[String], desc: Option[String], status: Option[TodoStatus.Value]) : Todo =
    this.copy(title = title.getOrElse(this.title),
              desc = desc.getOrElse(this.desc),
              status = status.getOrElse(this.status))
}





class TodoRepo @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db

  import dbConfig.driver.api._

  private val Todos = TableQuery[TodoTable]

  private def _findById(id: Long): DBIO[Option[Todo]] =
    Todos.filter(_.id === id).result.headOption

  def findById(id: Long): Future[Option[Todo]] =
    db.run(_findById(id))

  def findByOpenStatus: DBIO[List[Todo]] =
    Todos.filter(_.status === TodoStatus.open).to[List].result


  def create(title: String): Future[Long] = {
    val todo = Todo(0, title, "", TodoStatus.open, new DateTime(System.currentTimeMillis()))
    db.run(Todos returning Todos.map(_.id) += todo)
  }

  def insert(todo: Todo): DBIO[Long] =
    Todos returning Todos.map(_.id) += todo

  def all: Future[List[Todo]] =
    db.run(Todos.to[List].result)


  private class TodoTable(tag: Tag) extends Table[Todo](tag, "TODO") {
    def id = column[Long]("ID", O.AutoInc, O.PrimaryKey)
    def title = column[String]("TITLE")
    def desc = column[String]("DESC")
    def status = column[TodoStatus.Value]("STATUS")
    def createdDate = column[DateTime]("CREATED_DATE")

    def * = (id, title, desc, status, createdDate) <> (Todo.tupled, Todo.unapply)
    def ? = (id.?, title.?, desc.?, status.?, createdDate.?).shaped.<>({ r => import r._; _1.map(_ => Todo.tupled((_1.get, _2.get, _3.get, _4.get, _5.get))) }, (_: Any) => throw new Exception("Inserting into ? Todo not supported."))
  }

  implicit val todoStatusColumnType = MappedColumnType.base[TodoStatus.Value, String](
    _.toString, string => TodoStatus.withName(string))

}