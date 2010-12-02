import io._

object ex1 {
	def magicDef(title:String,magic:String)(block: => Unit):Unit = {
		println("==> "+title)
		block
	}

	def main(args:Array[String]):Unit = {
		magicDef("bleh","da magic") {			
			val x = 1
			println(x)
			println("INSIDE")
		}
		
	}		
}