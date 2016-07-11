package byteparse
package classparse

import fastparse.Base64.Decoder
import fastparse.allByte._
import utest._

import scala.collection.mutable.ArrayBuffer


object ClassTests extends TestSuite {
  import ClassParser.Ast._
  import ClassParser.BasicElems._
  import ClassAttributes._
  import CodeParser._

  val tests = TestSuite {

    'basic {
      'book {
        val classFile = strToBytes("CA FE BA BE " +
          "00 00 00 34 00 1D 0A 00 05 00 18 09 00 04 00 19 09 00 04 00 1A 07 00 " +
          "1B 07 00 1C 01 00 05 74 69 74 6C 65 01 00 12 4C 6A 61 76 61 2F 6C 61 " +
          "6E 67 2F 53 74 72 69 6E 67 3B 01 00 07 70 75 62 59 65 61 72 01 00 01 " +
          "49 01 00 06 3C 69 6E 69 74 3E 01 00 03 28 29 56 01 00 04 43 6F 64 65 " +
          "01 00 0F 4C 69 6E 65 4E 75 6D 62 65 72 54 61 62 6C 65 01 00 08 67 65 " +
          "74 54 69 74 6C 65 01 00 14 28 29 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53 " +
          "74 72 69 6E 67 3B 01 00 0A 67 65 74 50 75 62 59 65 61 72 01 00 03 28 " +
          "29 49 01 00 08 73 65 74 54 69 74 6C 65 01 00 15 28 4C 6A 61 76 61 2F " +
          "6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 56 01 00 0A 73 65 74 50 75 62 " +
          "59 65 61 72 01 00 04 28 49 29 56 01 00 0A 53 6F 75 72 63 65 46 69 6C " +
          "65 01 00 09 42 6F 6F 6B 2E 6A 61 76 61 0C 00 0A 00 0B 0C 00 06 00 07 " +
          "0C 00 08 00 09 01 00 04 42 6F 6F 6B 01 00 10 6A 61 76 61 2F 6C 61 6E " +
          "67 2F 4F 62 6A 65 63 74 00 20 00 04 00 05 00 00 00 02 00 02 00 06 00 " +
          "07 00 00 00 02 00 08 00 09 00 00 00 05 00 00 00 0A 00 0B 00 01 00 0C " +
          "00 00 00 1D 00 01 00 01 00 00 00 05 2A B7 00 01 B1 00 00 00 01 00 0D " +
          "00 00 00 06 00 01 00 00 00 01 00 01 00 0E 00 0F 00 01 00 0C 00 00 00 " +
          "1D 00 01 00 01 00 00 00 05 2A B4 00 02 B0 00 00 00 01 00 0D 00 00 00 " +
          "06 00 01 00 00 00 06 00 01 00 10 00 11 00 01 00 0C 00 00 00 1D 00 01 " +
          "00 01 00 00 00 05 2A B4 00 03 AC 00 00 00 01 00 0D 00 00 00 06 00 01 " +
          "00 00 00 0A 00 01 00 12 00 13 00 01 00 0C 00 00 00 22 00 02 00 02 00 " +
          "00 00 06 2A 2B B5 00 02 B1 00 00 00 01 00 0D 00 00 00 0A 00 02 00 00 " +
          "00 0E 00 05 00 0F 00 01 00 14 00 15 00 01 00 0C 00 00 00 22 00 02 00 " +
          "02 00 00 00 06 2A 1B B5 00 03 B1 00 00 00 01 00 0D 00 00 00 0A 00 02 " +
          "00 00 00 12 00 05 00 13 00 01 00 16 00 00 00 02 00 17")
        /* Book.class from compiled class Book.java */

        val Parsed.Success(parsedClassInfo, _) = ClassParser.classFile.parse(classFile)
        val parsedClass = ClassParser.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass == ClassFile(0, 52,
          ClassFlags(false, false, true /*accSuper*/, false, false, false, false, false),
          ArrayBuffer(
            MethodRef("<init>", "()V", Class("java/lang/Object")),
            FieldRef("title", "Ljava/lang/String;", Class("Book")),
            FieldRef("pubYear", "I", Class("Book")),
            Class("Book"),
            Class("java/lang/Object"),
            BasicElemPool(StringElem("title")),
            BasicElemPool(StringElem("Ljava/lang/String;")),
            BasicElemPool(StringElem("pubYear")),
            BasicElemPool(StringElem("I")),
            BasicElemPool(StringElem("<init>")),
            BasicElemPool(StringElem("()V")),
            BasicElemPool(StringElem("Code")),
            BasicElemPool(StringElem("LineNumberTable")),
            BasicElemPool(StringElem("getTitle")),
            BasicElemPool(StringElem("()Ljava/lang/String;")),
            BasicElemPool(StringElem("getPubYear")),
            BasicElemPool(StringElem("()I")),
            BasicElemPool(StringElem("setTitle")),
            BasicElemPool(StringElem("(Ljava/lang/String;)V")),
            BasicElemPool(StringElem("setPubYear")),
            BasicElemPool(StringElem("(I)V")),
            BasicElemPool(StringElem("SourceFile")),
            BasicElemPool(StringElem("Book.java")),
            NameAndType("<init>", "()V"),
            NameAndType("title", "Ljava/lang/String;"),
            NameAndType("pubYear", "I"),
            BasicElemPool(StringElem("Book")),
            BasicElemPool(StringElem("java/lang/Object"))),
          Class("Book"),
          Some(Class("java/lang/Object")),
          ArrayBuffer(),
          ArrayBuffer(
            Field("title", "Ljava/lang/String;",
              FieldFlags(false, true /*accPrivate*/, false, false, false, false, false, false, false),
              ArrayBuffer()),
            Field("pubYear", "I",
              FieldFlags(false, true /*accPrivate*/, false, false, false, false, false, false, false),
              ArrayBuffer())),
          ArrayBuffer(
            Method("<init>", "()V",
              MethodFlags(false, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0(), InvokeSpecial(1), Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 01")))))),
            Method("getTitle", "()Ljava/lang/String;",
              MethodFlags(true /*accPublic*/, false, false, false, false,
                          false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0(), GetField(2), AReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 06")))))),
            Method("getPubYear", "()I",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                          false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0(), GetField(3), IReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 0A")))))),
            Method("setTitle", "(Ljava/lang/String;)V",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                          false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(ALoad0(), ALoad1(), PutField(2), Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 0E 00 05 00 0F")))))),
            Method("setPubYear","(I)V",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                          false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(ALoad0(), ILoad1(), PutField(3), Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 12 00 05 00 13"))))))),
          ArrayBuffer(SourceFileAttribute("Book.java"))))
      }

      'book2 {
        val classFile = strToBytes("CA FE BA BE " +
          "00 00 00 34 00 4D 0A 00 12 00 34 09 00 11 00 35 09 00 11 00 36 09 " +
          "00 11 00 37 09 00 11 00 38 08 00 39 08 00 3A 08 00 3B 08 00 3C 07 " +
          "00 3D 0A 00 0A 00 34 0A 00 0A 00 3E 08 00 3F 0A 00 40 00 41 0A 00 " +
          "13 00 42 0A 00 0A 00 43 07 00 44 07 00 45 07 00 46 01 00 05 47 65 " +
          "6E 72 65 01 00 0C 49 6E 6E 65 72 43 6C 61 73 73 65 73 01 00 05 74 " +
          "69 74 6C 65 01 00 12 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 " +
          "6E 67 3B 01 00 07 70 75 62 59 65 61 72 01 00 01 49 01 00 05 67 65 " +
          "6E 72 65 01 00 0D 4C 42 6F 6F 6B 32 24 47 65 6E 72 65 3B 01 00 06 " +
          "63 6F 70 69 65 73 01 00 06 3C 69 6E 69 74 3E 01 00 03 28 29 56 01 " +
          "00 04 43 6F 64 65 01 00 0F 4C 69 6E 65 4E 75 6D 62 65 72 54 61 62 " +
          "6C 65 01 00 08 67 65 74 54 69 74 6C 65 01 00 14 28 29 4C 6A 61 76 " +
          "61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 01 00 0A 67 65 74 50 75 " +
          "62 59 65 61 72 01 00 03 28 29 49 01 00 08 67 65 74 47 65 6E 67 65 " +
          "01 00 0F 28 29 4C 42 6F 6F 6B 32 24 47 65 6E 72 65 3B 01 00 09 67 " +
          "65 74 43 6F 70 69 65 73 01 00 08 73 65 74 54 69 74 6C 65 01 00 15 " +
          "28 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 56 01 " +
          "00 0A 73 65 74 50 75 62 59 65 61 72 01 00 04 28 49 29 56 01 00 08 " +
          "73 65 74 47 65 6E 72 65 01 00 10 28 4C 42 6F 6F 6B 32 24 47 65 6E " +
          "72 65 3B 29 56 01 00 09 73 65 74 43 6F 70 69 65 73 01 00 08 74 6F " +
          "53 74 72 69 6E 67 01 00 0D 53 74 61 63 6B 4D 61 70 54 61 62 6C 65 " +
          "07 00 47 01 00 0A 53 6F 75 72 63 65 46 69 6C 65 01 00 0A 42 6F 6F " +
          "6B 32 2E 6A 61 76 61 0C 00 1D 00 1E 0C 00 16 00 17 0C 00 18 00 19 " +
          "0C 00 1A 00 1B 0C 00 1C 00 19 01 00 09 4E 6F 20 63 6F 70 69 65 73 " +
          "01 00 0D 4F 6E 6C 79 20 6F 6E 65 20 63 6F 70 79 01 00 0A 54 77 6F " +
          "20 63 6F 70 69 65 73 01 00 10 41 20 6C 6F 74 20 6F 66 20 63 6F 70 " +
          "69 65 73 21 01 00 17 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E " +
          "67 42 75 69 6C 64 65 72 0C 00 48 00 49 01 00 01 20 07 00 47 0C 00 " +
          "4A 00 4B 0C 00 4C 00 22 0C 00 2F 00 22 01 00 05 42 6F 6F 6B 32 01 " +
          "00 10 6A 61 76 61 2F 6C 61 6E 67 2F 4F 62 6A 65 63 74 01 00 0B 42 " +
          "6F 6F 6B 32 24 47 65 6E 72 65 01 00 10 6A 61 76 61 2F 6C 61 6E 67 " +
          "2F 53 74 72 69 6E 67 01 00 06 61 70 70 65 6E 64 01 00 2D 28 4C 6A " +
          "61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 4C 6A 61 76 61 " +
          "2F 6C 61 6E 67 2F 53 74 72 69 6E 67 42 75 69 6C 64 65 72 3B 01 00 " +
          "07 76 61 6C 75 65 4F 66 01 00 15 28 49 29 4C 6A 61 76 61 2F 6C 61 " +
          "6E 67 2F 53 74 72 69 6E 67 3B 01 00 04 6E 61 6D 65 00 20 00 11 00 " +
          "12 00 00 00 04 00 02 00 16 00 17 00 00 00 02 00 18 00 19 00 00 00 " +
          "02 00 1A 00 1B 00 00 00 02 00 1C 00 19 00 00 00 0A 00 00 00 1D 00 " +
          "1E 00 01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B7 00 01 B1 " +
          "00 00 00 01 00 20 00 00 00 06 00 01 00 00 00 01 00 01 00 21 00 22 " +
          "00 01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 02 B0 00 " +
          "00 00 01 00 20 00 00 00 06 00 01 00 00 00 0A 00 01 00 23 00 24 00 " +
          "01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 03 AC 00 00 " +
          "00 01 00 20 00 00 00 06 00 01 00 00 00 0E 00 01 00 25 00 26 00 01 " +
          "00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 04 B0 00 00 00 " +
          "01 00 20 00 00 00 06 00 01 00 00 00 12 00 01 00 27 00 24 00 01 00 " +
          "1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 05 AC 00 00 00 01 " +
          "00 20 00 00 00 06 00 01 00 00 00 16 00 01 00 28 00 29 00 01 00 1F " +
          "00 00 00 22 00 02 00 02 00 00 00 06 2A 2B B5 00 02 B1 00 00 00 01 " +
          "00 20 00 00 00 0A 00 02 00 00 00 1A 00 05 00 1B 00 01 00 2A 00 2B " +
          "00 01 00 1F 00 00 00 22 00 02 00 02 00 00 00 06 2A 1B B5 00 03 B1 " +
          "00 00 00 01 00 20 00 00 00 0A 00 02 00 00 00 1E 00 05 00 1F 00 01 " +
          "00 2C 00 2D 00 01 00 1F 00 00 00 22 00 02 00 02 00 00 00 06 2A 2B " +
          "B5 00 04 B1 00 00 00 01 00 20 00 00 00 0A 00 02 00 00 00 22 00 05 " +
          "00 23 00 01 00 2E 00 2B 00 01 00 1F 00 00 00 22 00 02 00 02 00 00 " +
          "00 06 2A 1B B5 00 05 B1 00 00 00 01 00 20 00 00 00 0A 00 02 00 00 " +
          "00 26 00 05 00 27 00 01 00 2F 00 22 00 01 00 1F 00 00 00 AC 00 02 " +
          "00 02 00 00 00 6E 2A B4 00 05 AA 00 00 00 00 00 00 2E 00 00 00 00 " +
          "00 00 00 02 00 00 00 1C 00 00 00 22 00 00 00 28 12 06 4C A7 00 12 " +
          "12 07 4C A7 00 0C 12 08 4C A7 00 06 12 09 4C BB 00 0A 59 B7 00 0B " +
          "2A B4 00 02 B6 00 0C 12 0D B6 00 0C 2A B4 00 03 B8 00 0E B6 00 0C " +
          "12 0D B6 00 0C 2A B4 00 04 B6 00 0F B6 00 0C 12 0D B6 00 0C 2B B6 " +
          "00 0C B6 00 10 B0 00 00 00 02 00 20 00 00 00 1A 00 06 00 00 00 2C " +
          "00 20 00 2D 00 26 00 2E 00 2C 00 2F 00 32 00 30 00 35 00 32 00 30 " +
          "00 00 00 0C 00 05 20 05 05 05 FC 00 02 07 00 31 00 02 00 32 00 00 " +
          "00 02 00 33 00 15 00 00 00 0A 00 01 00 13 00 11 00 14 40 19")
        /* Book2.class from compiled class Book2.java */

        val Parsed.Success(parsedClassInfo, _) = ClassParser.classFile.parse(classFile)
        val parsedClass = ClassParser.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass == ClassFile(0, 52,
          ClassFlags(false, false, true, false, false, false, false, false),
          ArrayBuffer(
            MethodRef("<init>", "()V", Class("java/lang/Object")),
            FieldRef("title", "Ljava/lang/String;", Class("Book2")),
            FieldRef("pubYear", "I", Class("Book2")),
            FieldRef("genre", "LBook2$Genre;", Class("Book2")),
            FieldRef("copies", "I", Class("Book2")),
            BasicElemPool(StringElem("No copies")),
            BasicElemPool(StringElem("Only one copy")),
            BasicElemPool(StringElem("Two copies")),
            BasicElemPool(StringElem("A lot of copies!")),
            Class("java/lang/StringBuilder"),
            MethodRef("<init>", "()V", Class("java/lang/StringBuilder")),
            MethodRef("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", Class("java/lang/StringBuilder")),
            BasicElemPool(StringElem(" ")),
            MethodRef("valueOf", "(I)Ljava/lang/String;", Class("java/lang/String")),
            MethodRef("name", "()Ljava/lang/String;", Class("Book2$Genre")),
            MethodRef("toString", "()Ljava/lang/String;", Class("java/lang/StringBuilder")),
            Class("Book2"),
            Class("java/lang/Object"),
            Class("Book2$Genre"),
            BasicElemPool(StringElem("Genre")),
            BasicElemPool(StringElem("InnerClasses")),
            BasicElemPool(StringElem("title")),
            BasicElemPool(StringElem("Ljava/lang/String;")),
            BasicElemPool(StringElem("pubYear")),
            BasicElemPool(StringElem("I")),
            BasicElemPool(StringElem("genre")),
            BasicElemPool(StringElem("LBook2$Genre;")),
            BasicElemPool(StringElem("copies")),
            BasicElemPool(StringElem("<init>")),
            BasicElemPool(StringElem("()V")),
            BasicElemPool(StringElem("Code")),
            BasicElemPool(StringElem("LineNumberTable")),
            BasicElemPool(StringElem("getTitle")),
            BasicElemPool(StringElem("()Ljava/lang/String;")),
            BasicElemPool(StringElem("getPubYear")),
            BasicElemPool(StringElem("()I")),
            BasicElemPool(StringElem("getGenge")),
            BasicElemPool(StringElem("()LBook2$Genre;")),
            BasicElemPool(StringElem("getCopies")),
            BasicElemPool(StringElem("setTitle")),
            BasicElemPool(StringElem("(Ljava/lang/String;)V")),
            BasicElemPool(StringElem("setPubYear")),
            BasicElemPool(StringElem("(I)V")),
            BasicElemPool(StringElem("setGenre")),
            BasicElemPool(StringElem("(LBook2$Genre;)V")),
            BasicElemPool(StringElem("setCopies")),
            BasicElemPool(StringElem("toString")),
            BasicElemPool(StringElem("StackMapTable")),
            Class("java/lang/String"),
            BasicElemPool(StringElem("SourceFile")),
            BasicElemPool(StringElem("Book2.java")),
            NameAndType("<init>", "()V"),
            NameAndType("title", "Ljava/lang/String;"),
            NameAndType("pubYear", "I"),
            NameAndType("genre", "LBook2$Genre;"),
            NameAndType("copies", "I"),
            BasicElemPool(StringElem("No copies")),
            BasicElemPool(StringElem("Only one copy")),
            BasicElemPool(StringElem("Two copies")),
            BasicElemPool(StringElem("A lot of copies!")),
            BasicElemPool(StringElem("java/lang/StringBuilder")),
            NameAndType("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;"),
            BasicElemPool(StringElem(" ")),
            Class("java/lang/String"),
            NameAndType("valueOf", "(I)Ljava/lang/String;"),
            NameAndType("name", "()Ljava/lang/String;"),
            NameAndType("toString", "()Ljava/lang/String;"),
            BasicElemPool(StringElem("Book2")),
            BasicElemPool(StringElem("java/lang/Object")),
            BasicElemPool(StringElem("Book2$Genre")),
            BasicElemPool(StringElem("java/lang/String")),
            BasicElemPool(StringElem("append")),
            BasicElemPool(StringElem("(Ljava/lang/String;)Ljava/lang/StringBuilder;")),
            BasicElemPool(StringElem("valueOf")),
            BasicElemPool(StringElem("(I)Ljava/lang/String;")),
            BasicElemPool(StringElem("name"))),
          Class("Book2"),
          Some(Class("java/lang/Object")),
          ArrayBuffer(),
          ArrayBuffer(
            Field(
              "title",
              "Ljava/lang/String;",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()),
            Field(
              "pubYear",
              "I",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()),
            Field(
              "genre",
              "LBook2$Genre;",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()),
            Field(
              "copies",
              "I",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer())),
          ArrayBuffer(
            Method(
              "<init>",
              "()V",
              MethodFlags(false, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0(),
                    InvokeSpecial(1),
                    Return()
                  ),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 01")))))),
            Method(
              "getTitle",
              "()Ljava/lang/String;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0(),
                    GetField(2),
                    AReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 0a")))))),
            Method(
              "getPubYear",
              "()I",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0(),
                    GetField(3),
                    IReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 0e")))))),
            Method(
              "getGenge",
              "()LBook2$Genre;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0(),
                    GetField(4),
                    AReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 12")))))),
            Method(
              "getCopies",
              "()I",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0(),
                    GetField(5),
                    IReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 01 00 00 00 16")))))),
            Method(
              "setTitle",
              "(Ljava/lang/String;)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(ALoad0(),
                    ALoad1(),
                    PutField(2),
                    Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 1a 00 05 00 1b")))))),
            Method(
              "setPubYear",
              "(I)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0(),
                    ILoad1(),
                    PutField(3),
                    Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 1e 00 05 00 1f")))))),
            Method(
              "setGenre",
              "(LBook2$Genre;)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0(),
                    ALoad1(),
                    PutField(4),
                    Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 22 00 05 00 23")))))),
            Method(
              "setCopies",
              "(I)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0(),
                    ILoad1(),
                    PutField(5),
                    Return()),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", strToBytes("00 02 00 00 00 26 00 05 00 27")))))),
            Method(
              "toString",
              "()Ljava/lang/String;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0(),
                    GetField(5),
                    TableSwitch(46, 0, 2, ArrayBuffer(28, 34, 40)),
                    LDC(6),
                    AStore1(),
                    Goto(18),
                    LDC(7),
                    AStore1(),
                    Goto(12),
                    LDC(8),
                    AStore1(),
                    Goto(6),
                    LDC(9),
                    AStore1(),
                    New(10),
                    Dup(),
                    InvokeSpecial(11),
                    ALoad0(),
                    GetField(2),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad0(),
                    GetField(3),
                    InvokeStatic(14),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad0(),
                    GetField(4),
                    InvokeVirtual(15),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad1(),
                    InvokeVirtual(12),
                    InvokeVirtual(16),
                    AReturn()),
                  ArrayBuffer(),
                  ArrayBuffer(
                    BasicAttribute(
                      "LineNumberTable",
                      strToBytes("00 06 00 00 00 2c 00 20 00 2d 00 26 00 2e 00 2c 00 2f 00 32 00 30 00 35 00 32")),
                    BasicAttribute(
                      "StackMapTable",
                      strToBytes("00 05 20 05 05 05 fc 00 02 07 00 31"))))))),
          ArrayBuffer(
            SourceFileAttribute("Book2.java"),
            InnerClassesAttribute(
              ArrayBuffer(
                InnerClass(
                  Class("Book2$Genre"),
                  Some(Class("Book2")),
                  Some("Genre"),
                  InnerClassFlags(true, false, false, true, true, false, false, false, false, true)))))))
      }

      'attributes {
        val classFile = strToBytes("CA FE BA BE " +
          "00 00 00 34 00 27 0A 00 08 00 1E 07 00 1F 0A 00 02 00 20 07 00 21 " +
          "09 00 02 00 22 07 00 23 0A 00 06 00 1E 07 00 24 01 00 0A 49 6E 6E " +
          "65 72 43 6C 61 73 73 01 00 0C 49 6E 6E 65 72 43 6C 61 73 73 65 73 " +
          "01 00 09 43 4F 4E 53 54 5F 56 41 4C 01 00 01 49 01 00 0D 43 6F 6E " +
          "73 74 61 6E 74 56 61 6C 75 65 03 00 00 00 2A 01 00 06 3C 69 6E 69 " +
          "74 3E 01 00 03 28 29 56 01 00 04 43 6F 64 65 01 00 0F 4C 69 6E 65 " +
          "4E 75 6D 62 65 72 54 61 62 6C 65 01 00 06 6D 65 74 68 6F 64 01 00 " +
          "14 28 29 4C 6A 61 76 61 2F 6C 61 6E 67 2F 4F 62 6A 65 63 74 3B 01 " +
          "00 0A 45 78 63 65 70 74 69 6F 6E 73 01 00 0A 44 65 70 72 65 63 61 " +
          "74 65 64 01 00 09 53 69 67 6E 61 74 75 72 65 01 00 05 28 29 54 54 " +
          "3B 01 00 19 52 75 6E 74 69 6D 65 56 69 73 69 62 6C 65 41 6E 6E 6F " +
          "74 61 74 69 6F 6E 73 01 00 16 4C 6A 61 76 61 2F 6C 61 6E 67 2F 44 " +
          "65 70 72 65 63 61 74 65 64 3B 01 00 28 3C 54 3A 4C 6A 61 76 61 2F " +
          "6C 61 6E 67 2F 4F 62 6A 65 63 74 3B 3E 4C 6A 61 76 61 2F 6C 61 6E " +
          "67 2F 4F 62 6A 65 63 74 3B 01 00 0A 53 6F 75 72 63 65 46 69 6C 65 " +
          "01 00 12 41 74 74 72 69 62 75 74 65 54 65 73 74 2E 6A 61 76 61 0C " +
          "00 0F 00 10 01 00 18 41 74 74 72 69 62 75 74 65 54 65 73 74 24 49 " +
          "6E 6E 65 72 43 6C 61 73 73 0C 00 0F 00 25 01 00 0D 41 74 74 72 69 " +
          "62 75 74 65 54 65 73 74 0C 00 26 00 0C 01 00 13 6A 61 76 61 2F 69 " +
          "6F 2F 49 4F 45 78 63 65 70 74 69 6F 6E 01 00 10 6A 61 76 61 2F 6C " +
          "61 6E 67 2F 4F 62 6A 65 63 74 01 00 12 28 4C 41 74 74 72 69 62 75 " +
          "74 65 54 65 73 74 3B 29 56 01 00 0A 69 6E 6E 65 72 46 69 65 6C 64 " +
          "00 21 00 04 00 08 00 00 00 01 00 19 00 0B 00 0C 00 01 00 0D 00 00 " +
          "00 02 00 0E 00 02 00 01 00 0F 00 10 00 01 00 11 00 00 00 1D 00 01 " +
          "00 01 00 00 00 05 2A B7 00 01 B1 00 00 00 01 00 12 00 00 00 06 00 " +
          "01 00 00 00 03 00 01 00 13 00 14 00 05 00 11 00 00 00 37 00 03 00 " +
          "02 00 00 00 17 BB 00 02 59 2A B7 00 03 4C 2B 10 2A B5 00 05 BB 00 " +
          "06 59 B7 00 07 BF 00 00 00 01 00 12 00 00 00 0E 00 03 00 00 00 0C " +
          "00 09 00 0D 00 0F 00 0E 00 15 00 00 00 04 00 01 00 06 00 16 00 00 " +
          "00 00 00 17 00 00 00 02 00 18 00 19 00 00 00 06 00 01 00 1A 00 00 " +
          "00 03 00 17 00 00 00 02 00 1B 00 1C 00 00 00 02 00 1D 00 0A 00 00 " +
          "00 0A 00 01 00 02 00 04 00 09 00 01")
        /* AttributeTest.class from compiled class AttributeTest.java */


        val Parsed.Success(parsedClassInfo, _) = ClassParser.classFile.parse(classFile)
        val parsedClass = ClassParser.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass.fields == ArrayBuffer(
          Field(
            "CONST_VAL",
            "I",
            FieldFlags(true, false, false, true, true, false, false, false, false),
            ArrayBuffer(ConstantValueAttribute(IntElem(42))))))

        assert(parsedClass.methods(1) ==
          Method(
            "method",
            "()Ljava/lang/Object;",
            MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
            ArrayBuffer(
              CodeAttribute(3, 2,
                ArrayBuffer(
                  New(2),
                  Dup(),
                  ALoad0(),
                  InvokeSpecial(3),
                  AStore1(),
                  ALoad1(),
                  BIPush(42),
                  PutField(5),
                  New(6),
                  Dup(),
                  InvokeSpecial(7),
                  AThrow()),
                ArrayBuffer(),
                ArrayBuffer(
                  BasicAttribute("LineNumberTable", strToBytes("00 03 00 00 00 0c 00 09 00 0d 00 0f 00 0e")))),
              ExceptionsAttribute(ArrayBuffer(Class("java/io/IOException"))),
              DeprecatedAttribute(),
              SignatureAttribute("()TT;"),
              BasicAttribute("RuntimeVisibleAnnotations", strToBytes("00 01 00 1a 00 00")))))
      }
      'code {
        val classFile = ("yv66vgAAADQA2AoACgBoCQBpAGoIAGsKAGwAbQgAbggAbwoAbA" +
          "BwCABxCAByBwBzBwB0BkAJIftURC0YCgB1AHYKAGwAdwUAAAAAAAGGoARDaoAABkBe2ZmZmZmaB" +
          "kAFvwmVqveQBwB4CAB5CgAXAHoHAHsKABoAfAgAfQoAGgB6CAB+CAB/CACABwCBCACCCACDCACE" +
          "CACFBwCGCgAmAGgIAIcKACYAiAoAJgCJCgAmAIoIAIsIAIwIAI0IAI4IAI8IAJAKACYAkQgAkgg" +
          "AkwgAlAgAlQgAlggAlwgAmAgAmQgAmggAmwgAnAoAbACdCACeCACfCACgCAChCACiCACjCACkCA" +
          "ClCACmCACnCACoCACpCgAhAKoIAKsKACEArAgArQgArggArwgAsAgAsQgAsggAswgAtAoAtQC2C" +
          "gC1ALcHALgBAAY8aW5pdD4BAAMoKVYBAARDb2RlAQAPTGluZU51bWJlclRhYmxlAQAGbWV0aG9k" +
          "AQANU3RhY2tNYXBUYWJsZQcAuAcAeAcAewcAgQcAuQcAugcAuwEAClNvdXJjZUZpbGUBAA1Db2R" +
          "lVGVzdC5qYXZhDABZAFoHALwMAL0AvgEADEhlbGxvIFdvcmxkIQcAvwwAwADBAQAmSW50ZWdlcj" +
          "ogMTAgRG91YmxlOiAzLjE0IEJvb2xlYW46IHRydWUBAAZIZWxsbyAMAMIAwQEABVdvcmxkAQAJc" +
          "GkgPSAlLjVmAQAQamF2YS9sYW5nL09iamVjdAEADmphdmEvbGFuZy9NYXRoBwDDDADEAMUMAMYA" +
          "xwEAFGphdmEvbWF0aC9CaWdJbnRlZ2VyAQACNDIMAFkAwQEAFGphdmEvbWF0aC9CaWdEZWNpbWF" +
          "sDABZAMgBAAMwLjEBABJNeSBTdHJpbmcgSXMgSGVyZSEBACNQcmludGluZyBvbiBhIG5ldyBsaW" +
          "5lPwpObyBQcm9ibGVtIQEAJURvIHlvdSB3YW50IHRvIGFkZCBhIHRhYj8JTm8gUHJvYmxlbSEBA" +
          "BBqYXZhL2xhbmcvU3RyaW5nAQADQm9iAQAESm9obgEABEZyZWQBAApKdWFuIFBlZHJvAQAXamF2" +
          "YS9sYW5nL1N0cmluZ0J1aWxkZXIBAA5pbnRBcnJheSBAIDA6IAwAyQDKDADJAMsMAMwAzQEADml" +
          "udEFycmF5IEAgMTogAQAMCi0+T3BlcmF0b3JzAQAGMSsyID0gAQAGMi0xID0gAQAGMioxID0gAQ" +
          "AGMS8yID0gDADJAM4BAAgxMSUzID0gMgEADTMgPT0gMj8gZmFsc2UBAAwzICE9IDI/IHRydWUBA" +
          "AszID4gMj8gdHJ1ZQEADDMgPCAyPyBmYWxzZQEADDIgPD0gMj8gdHJ1ZQEADDIgPj0gMj8gdHJ1" +
          "ZQEAFTMgPiAyICYmIDIgPiAzPyBmYWxzZQEAFDMgPiAyIHx8IDIgPiAzPyB0cnVlAQAPISgzID0" +
          "9IDIpPyB0cnVlAQAWCi0+SW5jL0RlYy1yZW1lbnRhdGlvbgwAwADPAQAVCi0+Q29udHJvbCBTdH" +
          "J1Y3R1cmVzAQANSSBnZXQgcHJpbnRlZAEAB0kgZG9uJ3QBAAxJIGFsc28gZG9uJ3QBABBmb29Xa" +
          "GlsZSBWYWx1ZTogAQASZm9vRG9XaGlsZSBWYWx1ZTogAQAHSmFudWFyeQEACEZlYnJ1YXJ5AQAF" +
          "TWFyY2gBABBTb21lIG90aGVyIG1vbnRoAQAUU3dpdGNoIENhc2UgUmVzdWx0OiABAAVtYXliZQw" +
          "A0ADRAQADeWVzDADSANMBAAJubwEAEVlvdSBhbnN3ZXJlZCB5ZXMuAQAQWW91IGFuc3dlcmVkIG" +
          "5vLgEAE1lvdSBhbnN3ZXJlZCBtYXliZS4BAA1Zb3UgYW5zd2VyZWQgAQABQQEAAUIBAAMxMjMHA" +
          "NQMANUA1gwAzADXAQAIQ29kZVRlc3QBAAJbSQEAE1tMamF2YS9sYW5nL1N0cmluZzsBAAJbWgEA" +
          "EGphdmEvbGFuZy9TeXN0ZW0BAANvdXQBABVMamF2YS9pby9QcmludFN0cmVhbTsBABNqYXZhL2l" +
          "vL1ByaW50U3RyZWFtAQAHcHJpbnRsbgEAFShMamF2YS9sYW5nL1N0cmluZzspVgEABXByaW50AQ" +
          "AQamF2YS9sYW5nL0RvdWJsZQEAB3ZhbHVlT2YBABUoRClMamF2YS9sYW5nL0RvdWJsZTsBAAZwc" +
          "mludGYBADwoTGphdmEvbGFuZy9TdHJpbmc7W0xqYXZhL2xhbmcvT2JqZWN0OylMamF2YS9pby9Q" +
          "cmludFN0cmVhbTsBABooTGphdmEvbWF0aC9CaWdJbnRlZ2VyO0kpVgEABmFwcGVuZAEALShMamF" +
          "2YS9sYW5nL1N0cmluZzspTGphdmEvbGFuZy9TdHJpbmdCdWlsZGVyOwEAHChJKUxqYXZhL2xhbm" +
          "cvU3RyaW5nQnVpbGRlcjsBAAh0b1N0cmluZwEAFCgpTGphdmEvbGFuZy9TdHJpbmc7AQAcKEQpT" +
          "GphdmEvbGFuZy9TdHJpbmdCdWlsZGVyOwEABChJKVYBAAhoYXNoQ29kZQEAAygpSQEABmVxdWFs" +
          "cwEAFShMamF2YS9sYW5nL09iamVjdDspWgEAEWphdmEvbGFuZy9JbnRlZ2VyAQAIcGFyc2VJbnQ" +
          "BABUoTGphdmEvbGFuZy9TdHJpbmc7KUkBABUoSSlMamF2YS9sYW5nL1N0cmluZzsAIQBYAAoAAA" +
          "AAAAIAAQBZAFoAAQBbAAAAHQABAAEAAAAFKrcAAbEAAAABAFwAAAAGAAEAAAAGAAEAXQBaAAEAW" +
          "wAAB3oABwAqAAAEy7IAAhIDtgAEsgACEgW2AASyAAISBrYAB7IAAhIItgAHsgACEgkEvQAKWQMU" +
          "AAy4AA5TtgAPVwQ2BARZPlk9PBBkNgURJxA2BhQAEDcHEhI4CRQAEzkKBDYMAzYNEEE2DhQAFTk" +
          "QuwAXWRIYtwAZOhK7ABpZGRIVBLcAGzoTuwAaWRIctwAdOhQSHjoVEh86FhIgOheyAAIZFbYABL" +
          "IAAhkWtgAEsgACGRe2AAQQCrwKOhgEvQAhOhkQZLwEOhoGvApZAxEjKE9ZBBED6E9ZBREFOU86G" +
          "we9ACFZAxIiU1kEEiNTWQUSJFNZBhIlUzocBrwEWQMEVFkEA1RZBQNUOh2yAAK7ACZZtwAnEii2" +
          "ACkZGAMutgAqtgArtgAEGRgEBE+yAAK7ACZZtwAnEiy2ACkZGAQutgAqtgArtgAEsgACEi22AAQ" +
          "ENh4FNh+yAAK7ACZZtwAnEi62ACkVHhUfYLYAKrYAK7YABLIAArsAJlm3ACcSL7YAKRUfFR5ktg" +
          "AqtgArtgAEsgACuwAmWbcAJxIwtgApFR8VHmi2ACq2ACu2AASyAAK7ACZZtwAnEjG2ACkVHhUfb" +
          "LYAKrYAK7YABLIAArsAJlm3ACcSMbYAKRUehxUfh2+2ADK2ACu2AASyAAISM7YABLIAAhI0tgAE" +
          "sgACEjW2AASyAAISNrYABLIAAhI3tgAEsgACEji2AASyAAISObYABLIAAhI6tgAEsgACEju2AAS" +
          "yAAISPLYABAM2ILIAAhI9tgAEsgACFSCEIAG2AD6yAAKEIAEVILYAPrIAAhUghCD/tgA+sgAChC" +
          "D/FSC2AD6yAAISP7YABBAKNiEVIRAKoAAOsgACEkC2AASnAB0VIRAKpAAOsgACEkG2AASnAAuyA" +
          "AISQrYABAM2IhUiEGSiABGyAAIVIrYAPoQiAaf/7rIAArsAJlm3ACcSQ7YAKRUitgAqtgArtgAE" +
          "AzYjsgACFSO2AD6EIwEVIxBkof/xsgACuwAmWbcAJxJEtgApFSO2ACq2ACu2AAQDNiQVJBAKogA" +
          "RsgACFSS2AD6EJAGn/+4DNiQVJBAKogAoAzYlFSUQCqIAGBUkCKAADBUlCKAABqcAD4QlAaf/54" +
          "QkAaf/1xAJvApZAwRPWQQFT1kFBk9ZBgdPWQcIT1kIEAZPWRAGEAdPWRAHEAhPWRAIEAlPOiQZJ" +
          "DolGSW+NiYDNicVJxUmogAYGSUVJy42KLIAAhUotgA+hCcBp//nBjYlFSWqAAAALgAAAAEAAAAD" +
          "AAAAGQAAACAAAAAnEkU6JqcAFRJGOianAA4SRzompwAHEkg6JrIAArsAJlm3ACcSSbYAKRkmtgA" +
          "ptgArtgAEEko6JxknOigCNikZKLYAS6sAAABOAAAAAwAADcEAAAAxAAHS5wAAACEGLexoAAAAQR" +
          "koEky2AE2ZACYDNimnACAZKBJOtgBNmQAWBDYppwAQGSgSSrYATZkABgU2KRUpqgAAADoAAAAAA" +
          "AAAAgAAABkAAAAkAAAAL7IAAhJPtgAEpwAzsgACElC2AASnACiyAAISUbYABKcAHbIAArsAJlm3" +
          "ACcSUrYAKRkntgAptgArtgAECDYoFSgQCqIACBJTpwAFElQ6KbIAAhkptgAEElW4AFZXEHu4AFd" +
          "XsQAAAAIAXAAAAb4AbwAAAAgACAAJABAACgAYAAsAIAAMADYADwA5ABEAPwATAEMAFABIABUATQ" +
          "AWAFEAFwBWABgAWQAZAFwAGgBgAB0AZQAeAHAAHwB9ACAAiAAhAIwAIgCQACMAlAAkAJwAJQCkA" +
          "CYArAAnALIAKAC4ACkAvgAqANUAKwDvACwBAAAtARwALgEhAC8BPQAwAUsAMQFoADIBhQAzAaIA" +
          "NAG/ADUB3gA2AeYANwHuADgB9gA5Af4AOgIGADsCDgA8AhYAPQIeAD4CJgA/Ai4AQAIxAEECOQB" +
          "CAkQAQwJPAEQCWgBFAmUARgJtAEcCcQBIAngASQKDAEoCigBLApUATQKdAE8CoABQAqcAUQKvAF" +
          "ICtQBUAs8AVQLSAFcC2gBYAt0AWQLkAFoC/gBbAwgAXAMQAFsDFgBfAyAAYAMqAGEDNgBiAzkAY" +
          "AM/AF8DRQBmA3YAZwOQAGgDmABnA54AagOhAGwDvABtA8AAbgPDAG8DxwBwA8oAcQPOAHID0QBz" +
          "A9UAdgPvAHcD8wB4BGgAegRwAHsEcwB9BHsAfgR+AIAEhgCBBIkAgwSjAIYEpgCHBLYAiAS+AIk" +
          "ExACKBMoAiwBeAAAA2QAf/wKDAB8HAF8BAQEBAQEEAgMBAQEAAwcAYAcAYQcAYQcAYgcAYgcAYg" +
          "cAYwcAZAcAZQcAYwcAZAcAZQEBAQEAABEH/AACART8ABwB/AAuAfoAFPwAAgH8AAkBFfoABfoAB" +
          "f8APAAlBwBfAQEBAQEBBAIDAQEBAAMHAGAHAGEHAGEHAGIHAGIHAGIHAGMHAGQHAGUHAGMHAGQH" +
          "AGUBAQEBAQEHAGMHAGMBAQAA+AAb/AAdAQYGBvwAAwcAYv4ASgcAYgcAYgEPDwwaCgoK+QAZ/AA" +
          "OAUEHAGIAAQBmAAAAAgBn").toByteArray
        /* CodeTest.class from compiled class CodeTest.java */


        val Parsed.Success(parsedClassInfo, _) = ClassParser.classFile.parse(classFile)
        val parsedClass = ClassParser.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass.methods(1).attributes(0).asInstanceOf[CodeAttribute].code ==
            ArrayBuffer(
              GetStatic(2),
              LDC(3),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(5),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(6),
              InvokeVirtual(7),
              GetStatic(2),
              LDC(8),
              InvokeVirtual(7),
              GetStatic(2),
              LDC(9),
              IConst1(),
              ANewArray(10),
              Dup(),
              IConst0(),
              LDC2W(12),
              InvokeStatic(14),
              AAStore(),
              InvokeVirtual(15),
              Pop(),
              IConst1(),
              IStore(4),
              IConst1(),
              Dup(),
              IStore3(),
              Dup(),
              IStore2(),
              IStore1(),
              BIPush(100),
              IStore(5),
              SIPush(10000),
              IStore(6),
              LDC2W(16),
              LStore(7),
              LDC(18),
              FStore(9),
              LDC2W(19),
              DStore(10),
              IConst1(),
              IStore(12),
              IConst0(),
              IStore(13),
              BIPush(65),
              IStore(14),
              LDC2W(21),
              DStore(16),
              New(23),
              Dup(),
              LDC(24),
              InvokeSpecial(25),
              AStore(18),
              New(26),
              Dup(),
              ALoad(18),
              ILoad(4),
              InvokeSpecial(27),
              AStore(19),
              New(26),
              Dup(),
              LDC(28),
              InvokeSpecial(29),
              AStore(20),
              LDC(30),
              AStore(21),
              LDC(31),
              AStore(22),
              LDC(32),
              AStore(23),
              GetStatic(2),
              ALoad(21),
              InvokeVirtual(4),
              GetStatic(2),
              ALoad(22),
              InvokeVirtual(4),
              GetStatic(2),
              ALoad(23),
              InvokeVirtual(4),
              BIPush(10),
              NewArray(10),
              AStore(24),
              IConst1(),
              ANewArray(33),
              AStore(25),
              BIPush(100),
              NewArray(4),
              AStore(26),
              IConst3(),
              NewArray(10),
              Dup(),
              IConst0(),
              SIPush(9000),
              IAStore(),
              Dup(),
              IConst1(),
              SIPush(1000),
              IAStore(),
              Dup(),
              IConst2(),
              SIPush(1337),
              IAStore(),
              AStore(27),
              IConst4(),
              ANewArray(33),
              Dup(),
              IConst0(),
              LDC(34),
              AAStore(),
              Dup(),
              IConst1(),
              LDC(35),
              AAStore(),
              Dup(),
              IConst2(),
              LDC(36),
              AAStore(),
              Dup(),
              IConst3(),
              LDC(37),
              AAStore(),
              AStore(28),
              IConst3(),
              NewArray(4),
              Dup(),
              IConst0(),
              IConst1(),
              BAStore(),
              Dup(),
              IConst1(),
              IConst0(),
              BAStore(),
              Dup(),
              IConst2(),
              IConst0(),
              BAStore(),
              AStore(29),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(40),
              InvokeVirtual(41),
              ALoad(24),
              IConst0(),
              IALoad(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              ALoad(24),
              IConst1(),
              IConst1(),
              IAStore(),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(44),
              InvokeVirtual(41),
              ALoad(24),
              IConst1(),
              IALoad(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(45),
              InvokeVirtual(4),
              IConst1(),
              IStore(30),
              IConst2(),
              IStore(31),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(46),
              InvokeVirtual(41),
              ILoad(30),
              ILoad(31),
              IAdd(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(47),
              InvokeVirtual(41),
              ILoad(31),
              ILoad(30),
              ISub(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(48),
              InvokeVirtual(41),
              ILoad(31),
              ILoad(30),
              IMul(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(49),
              InvokeVirtual(41),
              ILoad(30),
              ILoad(31),
              IDiv(),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(49),
              InvokeVirtual(41),
              ILoad(30),
              I2D(),
              ILoad(31),
              I2D(),
              DDiv(),
              InvokeVirtual(50),
              InvokeVirtual(43),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(51),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(52),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(53),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(54),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(55),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(56),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(57),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(58),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(59),
              InvokeVirtual(4),
              GetStatic(2),
              LDC(60),
              InvokeVirtual(4),
              IConst0(),
              IStore(32),
              GetStatic(2),
              LDC(61),
              InvokeVirtual(4),
              GetStatic(2),
              ILoad(32),
              IInc(32,1),
              InvokeVirtual(62),
              GetStatic(2),
              IInc(32,1),
              ILoad(32),
              InvokeVirtual(62),
              GetStatic(2),
              ILoad(32),
              IInc(32,-1),
              InvokeVirtual(62),
              GetStatic(2),
              IInc(32,-1),
              ILoad(32),
              InvokeVirtual(62),
              GetStatic(2),
              LDC(63),
              InvokeVirtual(4),
              BIPush(10),
              IStore(33),
              ILoad(33),
              BIPush(10),
              IfICmpNe(14),
              GetStatic(2),
              LDC(64),
              InvokeVirtual(4),
              Goto(29),
              ILoad(33),
              BIPush(10),
              IfICmpLe(14),
              GetStatic(2),
              LDC(65),
              InvokeVirtual(4),
              Goto(11),
              GetStatic(2),
              LDC(66),
              InvokeVirtual(4),
              IConst0(),
              IStore(34),
              ILoad(34),
              BIPush(100),
              IfICmpGe(17),
              GetStatic(2),
              ILoad(34),
              InvokeVirtual(62),
              IInc(34,1),
              Goto(-18),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(67),
              InvokeVirtual(41),
              ILoad(34),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              IConst0(),
              IStore(35),
              GetStatic(2),
              ILoad(35),
              InvokeVirtual(62),
              IInc(35,1),
              ILoad(35),
              BIPush(100),
              IfICmpLt(-15),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(68),
              InvokeVirtual(41),
              ILoad(35),
              InvokeVirtual(42),
              InvokeVirtual(43),
              InvokeVirtual(4),
              IConst0(),
              IStore(36),
              ILoad(36),
              BIPush(10),
              IfICmpGe(17),
              GetStatic(2),
              ILoad(36),
              InvokeVirtual(62),
              IInc(36,1),
              Goto(-18),
              IConst0(),
              IStore(36),
              ILoad(36),
              BIPush(10),
              IfICmpGe(40),
              IConst0(),
              IStore(37),
              ILoad(37),
              BIPush(10),
              IfICmpGe(24),
              ILoad(36),
              IConst5(),
              IfICmpNe(12),
              ILoad(37),
              IConst5(),
              IfICmpNe(6),
              Goto(15),
              IInc(37,1),
              Goto(-25),
              IInc(36,1),
              Goto(-41),
              BIPush(9),
              NewArray(10),
              Dup(),
              IConst0(),
              IConst1(),
              IAStore(),
              Dup(),
              IConst1(),
              IConst2(),
              IAStore(),
              Dup(),
              IConst2(),
              IConst3(),
              IAStore(),
              Dup(),
              IConst3(),
              IConst4(),
              IAStore(),
              Dup(),
              IConst4(),
              IConst5(),
              IAStore(),
              Dup(),
              IConst5(),
              BIPush(6),
              IAStore(),
              Dup(),
              BIPush(6),
              BIPush(7),
              IAStore(),
              Dup(),
              BIPush(7),
              BIPush(8),
              IAStore(),
              Dup(),
              BIPush(8),
              BIPush(9),
              IAStore(),
              AStore(36),
              ALoad(36),
              AStore(37),
              ALoad(37),
              ArrayLength(),
              IStore(38),
              IConst0(),
              IStore(39),
              ILoad(39),
              ILoad(38),
              IfICmpGe(24),
              ALoad(37),
              ILoad(39),
              IALoad(),
              IStore(40),
              GetStatic(2),
              ILoad(40),
              InvokeVirtual(62),
              IInc(39,1),
              Goto(-25),
              IConst3(),
              IStore(37),
              ILoad(37),
              TableSwitch(46, 1, 3, ArrayBuffer(25, 32, 39)),
              LDC(69),
              AStore(38),
              Goto(21),
              LDC(70),
              AStore(38),
              Goto(14),
              LDC(71),
              AStore(38),
              Goto(7),
              LDC(72),
              AStore(38),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(73),
              InvokeVirtual(41),
              ALoad(38),
              InvokeVirtual(41),
              InvokeVirtual(43),
              InvokeVirtual(4),
              LDC(74),
              AStore(39),
              ALoad(39),
              AStore(40),
              IConstM1(),
              IStore(41),
              ALoad(40),
              InvokeVirtual(75),
              LookUpSwitch(78, ArrayBuffer((3521,49), (119527,33), (103672936,65))),
              ALoad(40),
              LDC(76),
              InvokeVirtual(77),
              IfEq(38),
              IConst0(),
              IStore(41),
              Goto(32),
              ALoad(40),
              LDC(78),
              InvokeVirtual(77),
              IfEq(22),
              IConst1(),
              IStore(41),
              Goto(16),
              ALoad(40),
              LDC(74),
              InvokeVirtual(77),
              IfEq(6),
              IConst2(),
              IStore(41),
              ILoad(41),
              TableSwitch(58, 0, 2, ArrayBuffer(25, 36, 47)),
              GetStatic(2),
              LDC(79),
              InvokeVirtual(4),
              Goto(51),
              GetStatic(2),
              LDC(80),
              InvokeVirtual(4),
              Goto(40),
              GetStatic(2),
              LDC(81),
              InvokeVirtual(4),
              Goto(29),
              GetStatic(2),
              New(38),
              Dup(),
              InvokeSpecial(39),
              LDC(82),
              InvokeVirtual(41),
              ALoad(39),
              InvokeVirtual(41),
              InvokeVirtual(43),
              InvokeVirtual(4),
              IConst5(),
              IStore(40),
              ILoad(40),
              BIPush(10),
              IfICmpGe(8),
              LDC(83),
              Goto(5),
              LDC(84),
              AStore(41),
              GetStatic(2),
              ALoad(41),
              InvokeVirtual(4),
              LDC(85),
              InvokeStatic(86),
              Pop(),
              BIPush(123),
              InvokeStatic(87),
              Pop(),
              Return()))
      }
    }
  }
}
