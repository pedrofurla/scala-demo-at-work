import io._

object ex2 {

	abstract class People(name:String, age:Int, job:Option[String])
	case class Male(name:String, age:Int, job:Option[String]=None) extends People(name,age,job)
	case class Female(name:String, age:Int, job:Option[String]=None) extends People(name,age,job)
	
	def main(args:Array[String]):Unit = {
		val rawPeople = Source.fromFile("samples/data.txt").getLines().map { x => x.split(',') }
		val people = for(p <- rawPeople) yield p match {
			case Array(name, age, "M") => Male(name, Integer.parseInt(age))
		 	case Array(name, age, "F") => Female(name, Integer.parseInt(age))
			case Array(name, age, "M",job) => Male(name, Integer.parseInt(age),Some(job))
			 case Array(name, age, "F",job) => Female(name, Integer.parseInt(age),Some(job))
		}
	}
		
}