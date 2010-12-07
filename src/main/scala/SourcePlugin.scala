
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
    	  //println(unit.body)
    	  
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
	      case outerApply @ Apply(apply @ Apply( method @ Select(_,name),  args @ (arg1 :: arg2 :: Nil)) , interestingSource :: _ ) 
	      		if (name.toString == "magicDef") =>	       	 
	          val const = Constant("MORE MAGIC!")
	       	  val lit = Literal(const)
	       	  lit.pos = arg2.pos
	       	  lit.tpe = const.tpe
	       	  
	       	  { import scala.tools.nsc.io._
	         	val pos = interestingSource.pos
	         	println("Wanted source "+pos)
	         	DumpScalaCompiler.compile(tree.symbol.sourceFile.path,pos)
	        	//println(File(tree.symbol.sourceFile.path).lines.take(pos.line-1).reduceLeft(_ + "\n"+_)) 
	          } 
	       	  /*println("arg2: pos:"+arg2.pos+" tpe:"+arg2.tpe+" symbol:"+arg2.symbol)
	       	  println("lit: pos:"+lit.pos+" tpe:"+lit.tpe+" symbol:"+lit.symbol)
	       	  println("const:  tpe:"+const.tpe)*/
	          
	       	  val newApply = treeCopy.Apply(apply, method, arg1 :: lit :: Nil)
	       	  val newOuterApply = treeCopy.Apply(outerApply, newApply, List(interestingSource))
	       	  newOuterApply
	      case t =>        
	          super.transform(t)
	  }
  }
  
}

object DumpScalaCompiler { 
	val arg:String="";
	def errorFn(str: String) = Console println str

	val command = new scala.tools.nsc.GenericRunnerCommand(List(""), errorFn _)
    import command.settings
	
	import nsc.reporters._
	import nsc.interactive.RangePositions	
	val reporter = new ConsoleReporter(settings)
	
	object compiler extends Global(settings, reporter) with RangePositions {
	    override protected def computeInternalPhases() {
	      phasesSet += syntaxAnalyzer
	      phasesSet += analyzer.namerFactory
	      phasesSet += analyzer.packageObjects
	      phasesSet += analyzer.typerFactory
	      phasesSet += superAccessors
	      phasesSet += pickler
	      phasesSet += refchecks
	    }
	    override def onlyPresentation = true
	    //println(classPath)
	    import scala.tools.util._
	    //println(new PathResolver(settings).Calculated )
	}
	import scala.tools.nsc.util.Position
	def compile(arg:String, pos:Position) = {
	  println("Compiling "+arg)
	  val run = new compiler.Run()
	  run compile List(arg)
	  //println(run.units.foldLeft("")( _ + _.toString ))
	  import compiler.definitions.{ RootPackage => RP, EmptyPackage => EP}
	  
	  /*
	   OffsetPosition 
	   override def toString = {
229	    val pointmsg = if (point > source.length) "out-of-bounds-" else "offset="
230	    "source-%s,line-%s,%s%s".format(source.path, line, pointmsg, point)
231	  }
	   
	   override def toString = "RangePosition("+source+", "+start+", "+point+", "+end+")"
	   */
	   
	  for(u <- compiler.currentRun.units; b <- u.body; if b.pos == pos ) println(b.getClass.getSimpleName+ " " +b+ " " +b.pos)
	 /* for(c <- EP.info.members; if c.sourceFile!=null) {
	 	  println(c+" "+c.info.member(compiler.newTermName("main")).infosString)
	  }*/
	  
	}
	
}

object SourcePlugin {
	def main(args:Array[String]):Unit = { 
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