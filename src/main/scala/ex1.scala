import io._

object ex1 {
	val five = 5
	val amount = five / 0 
	def example(title:String)(block: => Unit):Unit = {
		println("==> "+title)
		block
	}
	def magicDef(title:String,magic:String)(block: => Unit):Unit = {
		println("==> "+title)
		block
	}
	// Inferencia de ;
	// Sem `return`

	def main(args:Array[String]):Unit = {
		example("Tuplas") {
			val t = ("Um string", 3.14, new java.io.File("dados.txt"))
			println(t)
			println(t._1 + " - " + t._2)			
		}
		
		example("Tudo são expressões") {
			def bool2str(x:Boolean) = if(x) "True" else "False" 
			println( bool2str(true) )
		}
		
		example("Closures / Funções são objetos") {
			val string2int : String => Int = (s:String) => Integer.parseInt(s)

			def converter(s:String, s2i:String => Int) = s2i(s)

			println( converter("123", string2int) )
		}
		
		example("Inferência de tipos") {
			val string2int = (s:String) => Integer.parseInt(s)
			// TODO REVER
			example("Parâmetros Implícitos") {
				def converter2(s:String)(implicit s2i:(String => Int)) = s2i(s)
	
				implicit val firstStr2int = (x:String) => string2int(x.charAt(0).toString)
	
				converter2("321")
	
				converter2("321")(string2int)			
			}
		}
		
		example("Aliases") {
			type FILE = java.io.File
			new FILE("aaa")

			import java.io.{File => F}
			new F("AAA")
		}

		example("Currying / Partial application") {
			val string2int  = Integer.parseInt(_:String)
		}
				
		example("Imports fáceis") { 
			import java.io.{File, FileInputStream => FIS}
			
			new FIS(new File("aaa"))
			
			import java.lang.Math			
			Math.abs(-10)
			
			import java.lang.Math.{PI, tan}			
			tan(PI)
			
			import java.lang.Math.{abs => absolute, tan => tangente}					
			absolute(-10)
						
			// Easy acessors and mutators
			class Address(var street:String, var number:Int)		
			val ideais=new Address("Rua da assembleia", 98)
			ideais.number
			
			ideais.number = 100
			
			ideais.number
			example("Imports de objetos") {     
				import ideais._		
				println(street)
			}
			
			example("Named arguments") {
				val oldIdeais=new Address(number=80, street="Rua Sao Jose")		
				println(oldIdeais.street)		
			}
		}
		
		// Default arguments
		def aaa(y:String, x:Int=1) = { println(1) }
		aaa("")
		
	}
	
	// Aparentemente default arguments não funcionam para classes locais
	// Default arguments		
	class Address(var street:String, var number:Int, var city:String="Rio de Janeiro")
	val ideais=new Address("Rua da assembleia", 98)
	ideais.city		

	/*
	example("") {
	}
	 */		
		
}