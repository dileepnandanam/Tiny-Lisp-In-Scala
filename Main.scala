package lisp

object Main 
{	var statment=""
	def main(args: Array[String])
	{
		while(statment != "exit" | statment !="quit")
		{	print("\n]=>")
			statment=readLine()
			if(statment!="")
			{
				var t=interpreter.parse(statment)
				print(interpreter.eval(t,interpreter.globals))
			}
		}
	}
}
object interpreter
{
	abstract class Expression
	case class Variable(val name:String) extends Expression
	case class Value(val x:Any) extends Expression
	case class FunCall(val name:Expression, val args:List[Expression]) extends Expression
	class Env(var env:scala.collection.mutable.Map[String,Any],var outer:Env)
	{
		def set(v:String,e:Any)
		{
			env(v)=e
		}
		
		def get(v:String):Any={
			if(env.contains(v))
				env(v)
			else if(outer==null)
				return("unbound variable "+v.toString)
			else outer.get(v)
		}
	}
	
	
	var builtin=scala.collection.mutable.Map[String,Any](
		("+",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a+b }),
		("-",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a-b }),
		("*",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a*b }),
		("/",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a/b }),
		("=",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a==b }),
		(">",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a>b }),
		("<",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a<b }),
		(">=",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a>=b }),
		("<=",(a:Any,b:Any)=>(a,b) match {case (a:Int,b:Int) => a<=b })
	)
	var globals=new Env(builtin,null)
	
	def split(s:String,sep:String)=
	{
		def sp(e:String,s:String,l:List[String],i:Int,n:Int):List[String]=
		{   
			if(i+s.length>=e.length) l:::List(e)
			else if(e.substring(i,i+s.length)==s & n==0) sp(e.substring(i+s.length,e.length),s,l:::List(e.substring(0,i)),0,n)
			else if(e(i)=='[' | e(i)=='(') sp(e,s,l,i+1,n+1)
			else if(e(i)==']' | e(i)==')') sp(e,s,l,i+1,n-1)
			else sp(e,s,l,i+1,n)
		}
	
		sp(s,sep,List[String](),0,0)
	}
	
	def parse(code:String):Expression=
	{	
		val exp= if(code(0)=='(') code.substring(1,code.length-1) else code

		if(exp.forall((x:Char)=>x<=58 & x>=48))
			return(Value(exp.toInt))
		
		
		val exprs=split(exp," ")
		if(exprs.length==1)
			return(Variable(exprs(0)))
		else
		{
			return(FunCall(parse(exprs.head),(for(i <- exprs.tail) yield(parse(i))).toList))
		}
	}
	
	def eval(e:Any,env:Env):Any=
	{
		val res=e match {
			case v:Value => v.x
			case Variable(x) => env.get(x) match {case i:Int => i}
			
			
			case FunCall(name,arg) =>
			{	
				name match {
					case Variable("list") => arg.map(eval(_,env))
					case Variable("car") => eval(arg(0),env) match {case l:List[Any] => l.head}
					case Variable("cdr") => eval(arg(0),env) match {case l:List[Any] => l.tail}
					
					case Variable("display") => print(eval(arg(0),env))
					case Variable("set!") => env.set(arg(0) match {case Variable(name) => name},eval(arg(1),env))
					case Variable("begin") => {
						for(i<-arg) eval(i,env)
					}
					case Variable("define") => env.set(arg(0) match {case Variable(name) => name},eval(arg(1),env))
					case Variable("lambda") => {
						arg match {
							
							case List(v1:Variable,e:Expression) =>(x1:Any)=>eval(e,new Env(scala.collection.mutable.Map((v1.name,x1)),env))
							case List(v1:Variable,v2:Variable,e:Expression) =>(x1:Any,x2:Any)=>eval(e,new Env(scala.collection.mutable.Map((v1.name,x1),(v2.name,x2)),env))
							case List(v1:Variable,v2:Variable,v3:Variable,e:Expression) => (x1:Any,x2:Any,x3:Any)=>eval(e,new Env(scala.collection.mutable.Map((v1.name,x1),(v2.name,x2),(v3.name,x3)),env))
							
						}
					}
					case Variable("if") => {
						arg match {
							case List(c,b1,b2) => if(eval(c,env) match {case b:Boolean => b}) eval(b1,env) else eval(b2,env)
							case List(c,b1) => if(eval(c,env) match {case b:Boolean => b}) eval(b1,env) 
						}
				
					}
					case _ =>{
						val f = name match {case Variable(fname) => env.get(fname) case f:FunCall => eval(f,env)}
						f match { 
							case f:Function1[Any,Any] => f(eval(arg(0),env))
							case f:Function2[Any,Any,Any] => f(eval(arg(0),env),eval(arg(1),env))
							case f:Function3[Any,Any,Any,Any] => f(eval(arg(0),env),eval(arg(1),env),eval(arg(2),env))
							case f:Function4[Any,Any,Any,Any,Any] => f(eval(arg(0),env),eval(arg(1),env),eval(arg(2),env),eval(arg(3),env))
							// Require upto Function22 as scala standards					
						}
		
					}
				}	
					 			
			
					
			}
		
			
		}
		

		return res
	}
	
	
	
}
