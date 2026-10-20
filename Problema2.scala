object Problema2{

    def crearValidador(minimo: Int)(maximo: Int)(numero: Int): Boolean = {
        numero >= minimo && numero <= maximo
    }
    
    val validarPorcentaje: Int => Boolean = crearValidador(0)(100)
    val validarEdadLaboral: Int => Boolean = crearValidador(18)(65)
    val validarNota: Int => Boolean = crearValidador(0)(100)

    def evaluar(numero: Int, validador: Int => Boolean): String = {
        if validador(numero) then "Valido" else "Invalido"
    }

    def main(args: Array[String]): Unit = {
        println(evaluar(50, validarPorcentaje))
        println(evaluar(70, validarEdadLaboral))
        println(evaluar(34, validarEdadLaboral))
        println(evaluar(-10, validarNota))
    }
}