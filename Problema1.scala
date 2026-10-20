object Problema1{

    def crearTransformacion(
        factor: Double,
        desplazamiento: Double
    ): Double => Double = {
        (x: Double) => factor * x + desplazamiento
    }

    val corregirError: Double => Double = crearTransformacion(1, -1.5)
    val calibrarSensor: Double => Double = crearTransformacion(1.04, 0)
    val convertirFahrenheit: Double => Double = crearTransformacion(1.8, 32)
    val procesarTemperatura: Double => Double = 
        corregirError andThen calibrarSensor andThen convertirFahrenheit

    def main(args: Array[String]): Unit = {
        println(procesarTemperatura(20)) 
        println(procesarTemperatura(30)) 
        println(procesarTemperatura(10)) 
    }
}