import kotlin.collections.MutableMap
import java.util.Stack
import java.util.Vector

interface FileTreeEntry {
  abstract fun getSize(): Int
}

class File(size: Int) : FileTreeEntry {
  val filesize: Int = size

  override fun getSize(): Int {
    return filesize
  }
}

class Directory() : FileTreeEntry {
  val filetree = mutableMapOf<String, FileTreeEntry>()
  
  override fun getSize(): Int {
    var size = 0
    filetree.forEach { entry -> size += entry.value.getSize() }
    return size
  }

  fun appendFileTreeEntry(name: String, entry: FileTreeEntry) {
    filetree.put(name, entry)
  }

  fun getDirectory(name: String): Directory? {
    val entry = filetree.get(name)
    if (entry is Directory) {
      return entry
    }
    return null
  }

  fun getDeletableDirectoriesTotalSize(): Int {
    var size = 0;
    filetree.forEach { 
      entry -> run {
        val value = entry.value
        if (value is Directory) {
          size += value.getDeletableDirectoriesTotalSize()
        }
      }
    }
    val selfsize = getSize()
    if (selfsize <= 100000)
      size += selfsize
    return size
  }

  fun getValidDeletableDirectoriesSizes(minSpaceFreed: Int): Vector<Int> {
    val sizes = Vector<Int>()
    filetree.forEach {
      entry -> run {
        val value = entry.value
        if (value is Directory) {
          sizes.addAll(value.getValidDeletableDirectoriesSizes(minSpaceFreed))
        }
      }
    }
    val selfsize = getSize()
    if (selfsize >= minSpaceFreed)
      sizes.add(selfsize)
    return sizes
  }
}

fun main() {
  val filename = "input.txt"
  val lines: List<String> = java.io.File(filename).readLines()

  val root: Directory = Directory()
  var working_directories = Stack<Directory>()
  working_directories.push(root)

  lines.forEach { line -> run {
    val parts = line.split(" ")
    if (parts[0] == "$") { // command
      if (parts[1] == "cd") { // cd
        if (parts[2] == "..")
          working_directories.pop()
        else if (parts[2] != "/") // only the first line
          working_directories.push(working_directories.peek().getDirectory(parts[2])!!)
      } // for now we can ignore ls
    } else {
      if (parts[0] == "dir") { // directory
        working_directories.peek().appendFileTreeEntry(parts[1], Directory())
      } else { // file
        working_directories.peek().appendFileTreeEntry(parts[1], File(parts[0].toInt()))
      }
    }
  }}
  println("Part 1: ${root.getDeletableDirectoriesTotalSize()}")

  val unused_space = 70000000 - root.getSize()
  val minimum_freed_space = 30000000 - unused_space
  
  val sizes = root.getValidDeletableDirectoriesSizes(minimum_freed_space)
  var min = 70000000
  sizes.forEach { size -> run {
    if (size < min) min = size
  }}

  println("Part 2: ${min}")
}
