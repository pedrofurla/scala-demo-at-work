import io._

class SourceProvider extends StaticAnnotation

object sample {
	def magicDef(title:String,magic: String @SourceProvider)(block: => Unit):Unit = {
		println("==> "+title)
		println("magic begin:\n"+magic);
		println("-------------");
		block
	}

	magicDef("bleh","da magic") {
		println("ACheck1")
		println("ACheck2")
	}
	
	magicDef("bleh","da magic") {
		println("BCheck1")
	}
	
	magicDef("bleh","da magic") { println("CCheck1") }
	
	magicDef("bleh","da magic") 
	{
		println("DCheck1")
		println("DCheck2")    
	}
	
	
	def main(args:Array[String]):Unit = {
		magicDef("bleh","da magic") {			
			val x = 1
			println(x)
			println("INSIDE")
		}
		
	}		
}