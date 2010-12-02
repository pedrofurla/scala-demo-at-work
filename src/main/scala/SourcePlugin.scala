
import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class SourcePlugin(val global: Global) extends Plugin {
  import global._

  val name = "sourceplugin"
  val description = "checks for division by zero"
  val components = List[PluginComponent](Component)
  
  private object Component extends PluginComponent {
    val global: SourcePlugin.this.global.type = SourcePlugin.this.global   
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    val runsAfter = List[String]("refchecks");
    val phaseName = SourcePlugin.this.name
    def newPhase(_prev: Phase) = new SourcePluginPhase(_prev)    
    
    
    
    class SourcePluginPhase(prev: Phase) extends StdPhase(prev) {
      override def name = SourcePlugin.this.name
      
      def show(t:Tree)={
    	  println(t.productPrefix + " : "+ t.getClass)
      }
      def show(ts:List[Tree]):Unit={
    	  ts.foreach(show)
      }
      
      def apply(unit: CompilationUnit) {
    	  //DefDef (mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
    	  /*
    	  for (unit <- global.currentRun.units; if !unit.isJava) {
    	 	   unit.body = TreeTransformer.transform(unit.body)
    	  } */
    	  
    	  for ( /*tree @ Apply(Select(_, name), _) <- unit.body;*/
    	 		  /*if (name.toString contains "magicDef")*/
    	 		 tree @ Apply(Apply( Select(_,name), args @ _) , _ ) <- unit.body;
    	 		 if (name.toString == "magicDef")
              ) 
          {
    	 	println("tree:")
    	 	show(tree)
    	 	println(tree)
    	 	println("args:")
    	 	println(args)     
    	 	
            
          }
    	  
    	  //println(unit.body)
        /*for ( tree @ Apply(Select(rcvr, nme.DIV), List(Literal(Constant(0)))) <- unit.body;
             if rcvr.tpe <:< definitions.IntClass.tpe) 
          {
            unit.error(tree.pos, "definitely division by zero")
          }*/
      }
    }
  }
  object TreeTransformer extends Transformer {
    
	  /**
	   * This method transforms individual nodes of the tree.
	   */
	  override def transform(tree: Tree) = tree match {
	      case t @ Apply(method @ Select(_, name), args @ List(arg0 @ _, arg1 @ _)) if(name.toString == "magicDef") =>
	       	  Apply(method, List(arg0, transform(Literal(Constant("MORE MAGIC!"))) ))
	      case t =>        
	          super.transform(t)
	  }
  }
  object TreeTransformerTest extends Transformer {
    
	  /**
	   * This method transforms individual nodes of the tree.
	   */
	  override def transform(tree: Tree) = tree match {
	      case defDef @ DefDef(mods,name,tparams,vparams,tpt,impl) if(name.toString == "magicDef") => 
	          import scala.tools.nsc.symtab.Flags._
	          val tree = treeCopy.DefDef(defDef, mods | PRIVATE,name,tparams,vparams,tpt,transform(impl))
	          tree.symbol.setFlag(PRIVATE)              
	          tree
	      case t =>        
	          super.transform(t)
	  }
  }
}
object SourcePlugin {
	def main(args:Array[String]):Unit = { println("here I am");
		scala.tools.nsc.Main.main(
			Array(
				"src/main/scala/sample.scala",
				"-d","bin-tmp",
				"-Xplugin","lib/scala-plugin-xml.jar"
				/*"-Ybrowse:refchecks"*/
				)
			)
	}
}