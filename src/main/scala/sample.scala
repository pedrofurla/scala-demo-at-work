import io._

class SourceProvider extends StaticAnnotation

object sample {
	def magicDef(title:String,magic: String @SourceProvider)(block: => Unit):Unit = {
		println("==> "+title)
		block
	}

	magicDef("bleh","da magic") {		
		println("Check")
	}
	
	def main(args:Array[String]):Unit = {
		magicDef("bleh","da magic") {			
			val x = 1
			println(x)
			println("INSIDE")
		}
		
	}		
}