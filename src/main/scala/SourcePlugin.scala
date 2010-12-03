
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
    val runsAfter = List[String]("namer");
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
    	  
    	  for (unit <- global.currentRun.units; if !unit.isJava) {
    	 	   unit.body = TreeTransformer.transform(unit.body)
    	  } 
    	  println(unit.body)
    	  
    	  /*for ( 
    	 		 tree @ Apply(Apply( Select(_,name), args @ (arg1 :: arg2 :: Nil)) , _ ) <- unit.body;
    	 		 if (name.toString == "magicDef")
              ) 
          {
    	 	println("tree:")
    	 	show(tree)
    	 	println(tree)
    	 	println("args:")
    	 	show(args)
    	 	println(args)
    	 	println("arg1:")
    	 	show(arg1)
    	 	println(arg1)     
    	 	println("arg2:")
    	 	show(arg2)
    	 	println(arg2)
          }*/
    	  
    	  //
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
	      case outerApply @ Apply(apply @ Apply( method @ Select(_,name),  args @ (arg1 :: arg2 :: Nil)) , outerArgs @ _ ) 
	      		if (name.toString == "magicDef") =>
	       	  val const = Constant("MORE MAGIC!")
	       	  val lit = Literal(const)
	       	  lit.pos = arg2.pos
	       	  lit.tpe = const.tpe
	       	  //val lit = treeCopy.Literal(arg2,Constant("MORE MAGIC!"))
	       	  println("arg2: pos:"+arg2.pos+" tpe:"+arg2.tpe+" symbol:"+arg2.symbol)
	       	  println("lit: pos:"+lit.pos+" tpe:"+lit.tpe+" symbol:"+lit.symbol)
	       	  println("const:  tpe:"+const.tpe)
	       	  val newApply = treeCopy.Apply(apply, method, arg1 :: lit :: Nil)
	       	  val newOuterApply = treeCopy.Apply(outerApply, newApply, outerArgs)
	       	  newOuterApply
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
				/*"-Ybrowse:namer"*/
				)
			)
	}
}