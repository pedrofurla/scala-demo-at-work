import sbt._

class Build(info: ProjectInfo) extends DefaultProject(info) {
  lazy val hi = task { None }
  
  override def mainClass = Some("all")
}