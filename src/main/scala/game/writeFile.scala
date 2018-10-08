package game
 
import java.io._


object WriteToFile {
    /**
      * Writes into a file in the location specified, the content it has been given
      * @param location: String: name and location of the file on the computer
      * @param content: String: Content to write into the file
      */
    def writeToFile(location: String, content: String): Unit = {
        val startCsv: String = if (!(new File(location).exists)) "AI Name; score; AI Name2; score2\n" else ""
        val bw = new BufferedWriter(new FileWriter(location, new File (location).exists))
        bw.write(startCsv + content + "\n")
        bw.flush()
        bw.close()
    }
   
}

