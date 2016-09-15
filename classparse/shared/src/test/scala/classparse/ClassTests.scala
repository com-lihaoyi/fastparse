package classparse

import fastparse.utils.Base64.Decoder
import fastparse.byte.all._
import utest._

import scala.collection.mutable.ArrayBuffer


object ClassTests extends TestSuite {
  import ClassParse.Ast._
  import ClassParse.Ast.Nop
  import ClassParse.BasicElems._
  import ClassAttributes._
  import CodeParser._

  val tests = TestSuite {

    'basic {
      'book {
        val classFile = hex"""
          CA FE BA BE
          00 00 00 34 00 1D 0A 00 05 00 18 09 00 04 00 19 09 00 04 00 1A 07 00
          1B 07 00 1C 01 00 05 74 69 74 6C 65 01 00 12 4C 6A 61 76 61 2F 6C 61
          6E 67 2F 53 74 72 69 6E 67 3B 01 00 07 70 75 62 59 65 61 72 01 00 01
          49 01 00 06 3C 69 6E 69 74 3E 01 00 03 28 29 56 01 00 04 43 6F 64 65
          01 00 0F 4C 69 6E 65 4E 75 6D 62 65 72 54 61 62 6C 65 01 00 08 67 65
          74 54 69 74 6C 65 01 00 14 28 29 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53
          74 72 69 6E 67 3B 01 00 0A 67 65 74 50 75 62 59 65 61 72 01 00 03 28
          29 49 01 00 08 73 65 74 54 69 74 6C 65 01 00 15 28 4C 6A 61 76 61 2F
          6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 56 01 00 0A 73 65 74 50 75 62
          59 65 61 72 01 00 04 28 49 29 56 01 00 0A 53 6F 75 72 63 65 46 69 6C
          65 01 00 09 42 6F 6F 6B 2E 6A 61 76 61 0C 00 0A 00 0B 0C 00 06 00 07
          0C 00 08 00 09 01 00 04 42 6F 6F 6B 01 00 10 6A 61 76 61 2F 6C 61 6E
          67 2F 4F 62 6A 65 63 74 00 20 00 04 00 05 00 00 00 02 00 02 00 06 00
          07 00 00 00 02 00 08 00 09 00 00 00 05 00 00 00 0A 00 0B 00 01 00 0C
          00 00 00 1D 00 01 00 01 00 00 00 05 2A B7 00 01 B1 00 00 00 01 00 0D
          00 00 00 06 00 01 00 00 00 01 00 01 00 0E 00 0F 00 01 00 0C 00 00 00
          1D 00 01 00 01 00 00 00 05 2A B4 00 02 B0 00 00 00 01 00 0D 00 00 00
          06 00 01 00 00 00 06 00 01 00 10 00 11 00 01 00 0C 00 00 00 1D 00 01
          00 01 00 00 00 05 2A B4 00 03 AC 00 00 00 01 00 0D 00 00 00 06 00 01
          00 00 00 0A 00 01 00 12 00 13 00 01 00 0C 00 00 00 22 00 02 00 02 00
          00 00 06 2A 2B B5 00 02 B1 00 00 00 01 00 0D 00 00 00 0A 00 02 00 00
          00 0E 00 05 00 0F 00 01 00 14 00 15 00 01 00 0C 00 00 00 22 00 02 00
          02 00 00 00 06 2A 1B B5 00 03 B1 00 00 00 01 00 0D 00 00 00 0A 00 02
          00 00 00 12 00 05 00 13 00 01 00 16 00 00 00 02 00 17
        """
        /* Book.class from compiled class Book.java */

        val expected = ClassFile(0, 52,
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
            BasicElemPool(StringElem("java/lang/Object"))
          ),
          Class("Book"),
          Some(Class("java/lang/Object")),
          ArrayBuffer(),
          ArrayBuffer(
            Field("title", "Ljava/lang/String;",
              FieldFlags(false, true /*accPrivate*/, false, false, false, false, false, false, false),
              ArrayBuffer()
            ),
            Field("pubYear", "I",
              FieldFlags(false, true /*accPrivate*/, false, false, false, false, false, false, false),
              ArrayBuffer()
            )
          ),
          ArrayBuffer(
            Method("<init>", "()V",
              MethodFlags(false, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0, InvokeSpecial(1), Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 01"))
                )
              )
            ),
            Method("getTitle", "()Ljava/lang/String;",
              MethodFlags(true /*accPublic*/, false, false, false, false,
                false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0, GetField(2), AReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 06"))
                )
              )
            ),
            Method("getPubYear", "()I",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(ALoad0, GetField(3), IReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 0A"))
                )
              )
            ),
            Method("setTitle", "(Ljava/lang/String;)V",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(ALoad0, ALoad1, PutField(2), Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 0E 00 05 00 0F"))
                )
              )
            ),
            Method("setPubYear","(I)V",
              MethodFlags(true /*accPublic*/, false, false, false, false, false, false,
                false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(ALoad0, ILoad1, PutField(3), Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 12 00 05 00 13"))
                )
              )
            )
          ),
          ArrayBuffer(SourceFileAttribute("Book.java"))
        )

        val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parse(classFile)
        val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass == expected)

        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parseIterator(
            classFile.toArray.grouped(chunkSize).map(Bytes.view)
          )
          val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

          assert(parsedClass == expected)
        }
      }

      'book2 {
        val classFile = hex"""
          CA FE BA BE
          00 00 00 34 00 4D 0A 00 12 00 34 09 00 11 00 35 09 00 11 00 36 09
          00 11 00 37 09 00 11 00 38 08 00 39 08 00 3A 08 00 3B 08 00 3C 07
          00 3D 0A 00 0A 00 34 0A 00 0A 00 3E 08 00 3F 0A 00 40 00 41 0A 00
          13 00 42 0A 00 0A 00 43 07 00 44 07 00 45 07 00 46 01 00 05 47 65
          6E 72 65 01 00 0C 49 6E 6E 65 72 43 6C 61 73 73 65 73 01 00 05 74
          69 74 6C 65 01 00 12 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69
          6E 67 3B 01 00 07 70 75 62 59 65 61 72 01 00 01 49 01 00 05 67 65
          6E 72 65 01 00 0D 4C 42 6F 6F 6B 32 24 47 65 6E 72 65 3B 01 00 06
          63 6F 70 69 65 73 01 00 06 3C 69 6E 69 74 3E 01 00 03 28 29 56 01
          00 04 43 6F 64 65 01 00 0F 4C 69 6E 65 4E 75 6D 62 65 72 54 61 62
          6C 65 01 00 08 67 65 74 54 69 74 6C 65 01 00 14 28 29 4C 6A 61 76
          61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 01 00 0A 67 65 74 50 75
          62 59 65 61 72 01 00 03 28 29 49 01 00 08 67 65 74 47 65 6E 67 65
          01 00 0F 28 29 4C 42 6F 6F 6B 32 24 47 65 6E 72 65 3B 01 00 09 67
          65 74 43 6F 70 69 65 73 01 00 08 73 65 74 54 69 74 6C 65 01 00 15
          28 4C 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 56 01
          00 0A 73 65 74 50 75 62 59 65 61 72 01 00 04 28 49 29 56 01 00 08
          73 65 74 47 65 6E 72 65 01 00 10 28 4C 42 6F 6F 6B 32 24 47 65 6E
          72 65 3B 29 56 01 00 09 73 65 74 43 6F 70 69 65 73 01 00 08 74 6F
          53 74 72 69 6E 67 01 00 0D 53 74 61 63 6B 4D 61 70 54 61 62 6C 65
          07 00 47 01 00 0A 53 6F 75 72 63 65 46 69 6C 65 01 00 0A 42 6F 6F
          6B 32 2E 6A 61 76 61 0C 00 1D 00 1E 0C 00 16 00 17 0C 00 18 00 19
          0C 00 1A 00 1B 0C 00 1C 00 19 01 00 09 4E 6F 20 63 6F 70 69 65 73
          01 00 0D 4F 6E 6C 79 20 6F 6E 65 20 63 6F 70 79 01 00 0A 54 77 6F
          20 63 6F 70 69 65 73 01 00 10 41 20 6C 6F 74 20 6F 66 20 63 6F 70
          69 65 73 21 01 00 17 6A 61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E
          67 42 75 69 6C 64 65 72 0C 00 48 00 49 01 00 01 20 07 00 47 0C 00
          4A 00 4B 0C 00 4C 00 22 0C 00 2F 00 22 01 00 05 42 6F 6F 6B 32 01
          00 10 6A 61 76 61 2F 6C 61 6E 67 2F 4F 62 6A 65 63 74 01 00 0B 42
          6F 6F 6B 32 24 47 65 6E 72 65 01 00 10 6A 61 76 61 2F 6C 61 6E 67
          2F 53 74 72 69 6E 67 01 00 06 61 70 70 65 6E 64 01 00 2D 28 4C 6A
          61 76 61 2F 6C 61 6E 67 2F 53 74 72 69 6E 67 3B 29 4C 6A 61 76 61
          2F 6C 61 6E 67 2F 53 74 72 69 6E 67 42 75 69 6C 64 65 72 3B 01 00
          07 76 61 6C 75 65 4F 66 01 00 15 28 49 29 4C 6A 61 76 61 2F 6C 61
          6E 67 2F 53 74 72 69 6E 67 3B 01 00 04 6E 61 6D 65 00 20 00 11 00
          12 00 00 00 04 00 02 00 16 00 17 00 00 00 02 00 18 00 19 00 00 00
          02 00 1A 00 1B 00 00 00 02 00 1C 00 19 00 00 00 0A 00 00 00 1D 00
          1E 00 01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B7 00 01 B1
          00 00 00 01 00 20 00 00 00 06 00 01 00 00 00 01 00 01 00 21 00 22
          00 01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 02 B0 00
          00 00 01 00 20 00 00 00 06 00 01 00 00 00 0A 00 01 00 23 00 24 00
          01 00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 03 AC 00 00
          00 01 00 20 00 00 00 06 00 01 00 00 00 0E 00 01 00 25 00 26 00 01
          00 1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 04 B0 00 00 00
          01 00 20 00 00 00 06 00 01 00 00 00 12 00 01 00 27 00 24 00 01 00
          1F 00 00 00 1D 00 01 00 01 00 00 00 05 2A B4 00 05 AC 00 00 00 01
          00 20 00 00 00 06 00 01 00 00 00 16 00 01 00 28 00 29 00 01 00 1F
          00 00 00 22 00 02 00 02 00 00 00 06 2A 2B B5 00 02 B1 00 00 00 01
          00 20 00 00 00 0A 00 02 00 00 00 1A 00 05 00 1B 00 01 00 2A 00 2B
          00 01 00 1F 00 00 00 22 00 02 00 02 00 00 00 06 2A 1B B5 00 03 B1
          00 00 00 01 00 20 00 00 00 0A 00 02 00 00 00 1E 00 05 00 1F 00 01
          00 2C 00 2D 00 01 00 1F 00 00 00 22 00 02 00 02 00 00 00 06 2A 2B
          B5 00 04 B1 00 00 00 01 00 20 00 00 00 0A 00 02 00 00 00 22 00 05
          00 23 00 01 00 2E 00 2B 00 01 00 1F 00 00 00 22 00 02 00 02 00 00
          00 06 2A 1B B5 00 05 B1 00 00 00 01 00 20 00 00 00 0A 00 02 00 00
          00 26 00 05 00 27 00 01 00 2F 00 22 00 01 00 1F 00 00 00 AC 00 02
          00 02 00 00 00 6E 2A B4 00 05 AA 00 00 00 00 00 00 2E 00 00 00 00
          00 00 00 02 00 00 00 1C 00 00 00 22 00 00 00 28 12 06 4C A7 00 12
          12 07 4C A7 00 0C 12 08 4C A7 00 06 12 09 4C BB 00 0A 59 B7 00 0B
          2A B4 00 02 B6 00 0C 12 0D B6 00 0C 2A B4 00 03 B8 00 0E B6 00 0C
          12 0D B6 00 0C 2A B4 00 04 B6 00 0F B6 00 0C 12 0D B6 00 0C 2B B6
          00 0C B6 00 10 B0 00 00 00 02 00 20 00 00 00 1A 00 06 00 00 00 2C
          00 20 00 2D 00 26 00 2E 00 2C 00 2F 00 32 00 30 00 35 00 32 00 30
          00 00 00 0C 00 05 20 05 05 05 FC 00 02 07 00 31 00 02 00 32 00 00
          00 02 00 33 00 15 00 00 00 0A 00 01 00 13 00 11 00 14 40 19
        """
        /* Book2.class from compiled class Book2.java */

        val expected = ClassFile(0, 52,
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
              ArrayBuffer()
            ),
            Field(
              "pubYear",
              "I",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()
            ),
            Field(
              "genre",
              "LBook2$Genre;",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()
            ),
            Field(
              "copies",
              "I",
              FieldFlags(false, true, false, false, false, false, false, false, false),
              ArrayBuffer()
            )
          ),
          ArrayBuffer(
            Method(
              "<init>",
              "()V",
              MethodFlags(false, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0,
                    InvokeSpecial(1),
                    Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 01"))
                )
              )
            ),
            Method(
              "getTitle",
              "()Ljava/lang/String;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0,
                    GetField(2),
                    AReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 0a"))
                )
              )
            ),
            Method(
              "getPubYear",
              "()I",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0,
                    GetField(3),
                    IReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 0e"))
                )
              )
            ),
            Method(
              "getGenge",
              "()LBook2$Genre;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0,
                    GetField(4),
                    AReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 12"))
                )
              )
            ),
            Method(
              "getCopies",
              "()I",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(1, 1,
                  ArrayBuffer(
                    ALoad0,
                    GetField(5),
                    IReturn),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 01 00 00 00 16"))
                )
              )
            ),
            Method(
              "setTitle",
              "(Ljava/lang/String;)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0,
                    ALoad1,
                    PutField(2),
                    Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 1a 00 05 00 1b"))
                )
              )
            ),
            Method(
              "setPubYear",
              "(I)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0,
                    ILoad1,
                    PutField(3),
                    Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 1e 00 05 00 1f"))
                )
              )
            ),
            Method(
              "setGenre",
              "(LBook2$Genre;)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0,
                    ALoad1,
                    PutField(4),
                    Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 22 00 05 00 23"))
                )
              )
            ),
            Method(
              "setCopies",
              "(I)V",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0,
                    ILoad1,
                    PutField(5),
                    Return),
                  ArrayBuffer(),
                  ArrayBuffer(BasicAttribute("LineNumberTable", hex"00 02 00 00 00 26 00 05 00 27"))
                )
              )
            ),
            Method(
              "toString",
              "()Ljava/lang/String;",
              MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
              ArrayBuffer(
                CodeAttribute(2, 2,
                  ArrayBuffer(
                    ALoad0,
                    GetField(5),
                    TableSwitch(46, 0, 2, ArrayBuffer(28, 34, 40)),
                    LDC(6),
                    AStore1,
                    Goto(18),
                    LDC(7),
                    AStore1,
                    Goto(12),
                    LDC(8),
                    AStore1,
                    Goto(6),
                    LDC(9),
                    AStore1,
                    New(10),
                    Dup,
                    InvokeSpecial(11),
                    ALoad0,
                    GetField(2),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad0,
                    GetField(3),
                    InvokeStatic(14),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad0,
                    GetField(4),
                    InvokeVirtual(15),
                    InvokeVirtual(12),
                    LDC(13),
                    InvokeVirtual(12),
                    ALoad1,
                    InvokeVirtual(12),
                    InvokeVirtual(16),
                    AReturn),
                  ArrayBuffer(),
                  ArrayBuffer(
                    BasicAttribute(
                      "LineNumberTable",
                      hex"00 06 00 00 00 2c 00 20 00 2d 00 26 00 2e 00 2c 00 2f 00 32 00 30 00 35 00 32"
                    ),
                    BasicAttribute(
                      "StackMapTable",
                      hex"00 05 20 05 05 05 fc 00 02 07 00 31"
                    )
                  )
                )
              )
            )
          ),
          ArrayBuffer(
            SourceFileAttribute("Book2.java"),
            InnerClassesAttribute(
              ArrayBuffer(
                InnerClass(
                  Class("Book2$Genre"),
                  Some(Class("Book2")),
                  Some("Genre"),
                  InnerClassFlags(true, false, false, true, true, false, false, false, false, true)
                )
              )
            )
          )
        )

        val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parse(classFile)
        val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass == expected)

        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parseIterator(
            classFile.toArray.grouped(chunkSize).map(Bytes.view)
          )
          val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

          assert(parsedClass == expected)
        }
      }

      'attributes {
        val classFile = hex"""
          CA FE BA BE
          00 00 00 34 00 27 0A 00 08 00 1E 07 00 1F 0A 00 02 00 20 07 00 21
          09 00 02 00 22 07 00 23 0A 00 06 00 1E 07 00 24 01 00 0A 49 6E 6E
          65 72 43 6C 61 73 73 01 00 0C 49 6E 6E 65 72 43 6C 61 73 73 65 73
          01 00 09 43 4F 4E 53 54 5F 56 41 4C 01 00 01 49 01 00 0D 43 6F 6E
          73 74 61 6E 74 56 61 6C 75 65 03 00 00 00 2A 01 00 06 3C 69 6E 69
          74 3E 01 00 03 28 29 56 01 00 04 43 6F 64 65 01 00 0F 4C 69 6E 65
          4E 75 6D 62 65 72 54 61 62 6C 65 01 00 06 6D 65 74 68 6F 64 01 00
          14 28 29 4C 6A 61 76 61 2F 6C 61 6E 67 2F 4F 62 6A 65 63 74 3B 01
          00 0A 45 78 63 65 70 74 69 6F 6E 73 01 00 0A 44 65 70 72 65 63 61
          74 65 64 01 00 09 53 69 67 6E 61 74 75 72 65 01 00 05 28 29 54 54
          3B 01 00 19 52 75 6E 74 69 6D 65 56 69 73 69 62 6C 65 41 6E 6E 6F
          74 61 74 69 6F 6E 73 01 00 16 4C 6A 61 76 61 2F 6C 61 6E 67 2F 44
          65 70 72 65 63 61 74 65 64 3B 01 00 28 3C 54 3A 4C 6A 61 76 61 2F
          6C 61 6E 67 2F 4F 62 6A 65 63 74 3B 3E 4C 6A 61 76 61 2F 6C 61 6E
          67 2F 4F 62 6A 65 63 74 3B 01 00 0A 53 6F 75 72 63 65 46 69 6C 65
          01 00 12 41 74 74 72 69 62 75 74 65 54 65 73 74 2E 6A 61 76 61 0C
          00 0F 00 10 01 00 18 41 74 74 72 69 62 75 74 65 54 65 73 74 24 49
          6E 6E 65 72 43 6C 61 73 73 0C 00 0F 00 25 01 00 0D 41 74 74 72 69
          62 75 74 65 54 65 73 74 0C 00 26 00 0C 01 00 13 6A 61 76 61 2F 69
          6F 2F 49 4F 45 78 63 65 70 74 69 6F 6E 01 00 10 6A 61 76 61 2F 6C
          61 6E 67 2F 4F 62 6A 65 63 74 01 00 12 28 4C 41 74 74 72 69 62 75
          74 65 54 65 73 74 3B 29 56 01 00 0A 69 6E 6E 65 72 46 69 65 6C 64
          00 21 00 04 00 08 00 00 00 01 00 19 00 0B 00 0C 00 01 00 0D 00 00
          00 02 00 0E 00 02 00 01 00 0F 00 10 00 01 00 11 00 00 00 1D 00 01
          00 01 00 00 00 05 2A B7 00 01 B1 00 00 00 01 00 12 00 00 00 06 00
          01 00 00 00 03 00 01 00 13 00 14 00 05 00 11 00 00 00 37 00 03 00
          02 00 00 00 17 BB 00 02 59 2A B7 00 03 4C 2B 10 2A B5 00 05 BB 00
          06 59 B7 00 07 BF 00 00 00 01 00 12 00 00 00 0E 00 03 00 00 00 0C
          00 09 00 0D 00 0F 00 0E 00 15 00 00 00 04 00 01 00 06 00 16 00 00
          00 00 00 17 00 00 00 02 00 18 00 19 00 00 00 06 00 01 00 1A 00 00
          00 03 00 17 00 00 00 02 00 1B 00 1C 00 00 00 02 00 1D 00 0A 00 00
          00 0A 00 01 00 02 00 04 00 09 00 01
        """
        /* AttributeTest.class from compiled class AttributeTest.java */


        val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parse(classFile)
        val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass.fields == ArrayBuffer(
          Field(
            "CONST_VAL",
            "I",
            FieldFlags(true, false, false, true, true, false, false, false, false),
            ArrayBuffer(ConstantValueAttribute(IntElem(42)))
          )
        ))

        assert(parsedClass.methods(1) ==
          Method(
            "method",
            "()Ljava/lang/Object;",
            MethodFlags(true, false, false, false, false, false, false, false, false, false, false, false),
            ArrayBuffer(
              CodeAttribute(3, 2,
                ArrayBuffer(
                  New(2),
                  Dup,
                  ALoad0,
                  InvokeSpecial(3),
                  AStore1,
                  ALoad1,
                  BIPush(42),
                  PutField(5),
                  New(6),
                  Dup,
                  InvokeSpecial(7),
                  AThrow),
                ArrayBuffer(),
                ArrayBuffer(
                  BasicAttribute("LineNumberTable", hex"00 03 00 00 00 0c 00 09 00 0d 00 0f 00 0e")
                )
              ),
              ExceptionsAttribute(ArrayBuffer(Class("java/io/IOException"))),
              DeprecatedAttribute,
              SignatureAttribute("()TT;"),
              BasicAttribute("RuntimeVisibleAnnotations", hex"00 01 00 1a 00 00")
            )
          )
        )
      }
      'code {
        val classFile = Bytes(
          ("yv66vgAAADQA2AoACgBoCQBpAGoIAGsKAGwAbQgAbggAbwoAbA" +
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
          "OAUEHAGIAAQBmAAAAAgBn").toByteArray)
        /* CodeTest.class from compiled class CodeTest.java */

        val expectedPool = ArrayBuffer(
          MethodRef("<init>", "()V", Class("java/lang/Object")),
          FieldRef("out", "Ljava/io/PrintStream;", Class("java/lang/System")),
          BasicElemPool(StringElem("Hello World!")),
          MethodRef("println","(Ljava/lang/String;)V", Class("java/io/PrintStream")),
          BasicElemPool(StringElem("Integer: 10 Double: 3.14 Boolean: true")),
          BasicElemPool(StringElem("Hello ")),
          MethodRef("print", "(Ljava/lang/String;)V", Class("java/io/PrintStream")),
          BasicElemPool(StringElem("World")),
          BasicElemPool(StringElem("pi = %.5f")),
          Class("java/lang/Object"),
          Class("java/lang/Math"),
          BasicElemPool(DoubleElem(3.141592653589793)),
          Nop,
          MethodRef("valueOf", "(D)Ljava/lang/Double;", Class("java/lang/Double")),
          MethodRef(
            "printf",
            "(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;",
            Class("java/io/PrintStream")
          ),
          BasicElemPool(LongElem(100000L)),
          Nop,
          BasicElemPool(FloatElem(234.5f)),
          BasicElemPool(DoubleElem(123.4)),
          Nop,
          BasicElemPool(DoubleElem(2.71828)),
          Nop,
          Class("java/math/BigInteger"),
          BasicElemPool(StringElem("42")),
          MethodRef("<init>", "(Ljava/lang/String;)V", Class("java/math/BigInteger")),
          Class("java/math/BigDecimal"),
          MethodRef("<init>", "(Ljava/math/BigInteger;I)V", Class("java/math/BigDecimal")),
          BasicElemPool(StringElem("0.1")),
          MethodRef("<init>", "(Ljava/lang/String;)V", Class("java/math/BigDecimal")),
          BasicElemPool(StringElem("My String Is Here!")),
          BasicElemPool(StringElem("Printing on a new line?\nNo Problem!")),
          BasicElemPool(StringElem("Do you want to add a tab?\tNo Problem!")),
          Class("java/lang/String"),
          BasicElemPool(StringElem("Bob")),
          BasicElemPool(StringElem("John")),
          BasicElemPool(StringElem("Fred")),
          BasicElemPool(StringElem("Juan Pedro")),
          Class("java/lang/StringBuilder"),
          MethodRef("<init>", "()V", Class("java/lang/StringBuilder")),
          BasicElemPool(StringElem("intArray @ 0: ")),
          MethodRef("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;", Class("java/lang/StringBuilder")),
          MethodRef("append", "(I)Ljava/lang/StringBuilder;", Class("java/lang/StringBuilder")),
          MethodRef("toString", "()Ljava/lang/String;", Class("java/lang/StringBuilder")),
          BasicElemPool(StringElem("intArray @ 1: ")),
          BasicElemPool(StringElem("\n->Operators")),
          BasicElemPool(StringElem("1+2 = ")),
          BasicElemPool(StringElem("2-1 = ")),
          BasicElemPool(StringElem("2*1 = ")),
          BasicElemPool(StringElem("1/2 = ")),
          MethodRef("append", "(D)Ljava/lang/StringBuilder;", Class("java/lang/StringBuilder")),
          BasicElemPool(StringElem("11%3 = 2")),
          BasicElemPool(StringElem("3 == 2? false")),
          BasicElemPool(StringElem("3 != 2? true")),
          BasicElemPool(StringElem("3 > 2? true")),
          BasicElemPool(StringElem("3 < 2? false")),
          BasicElemPool(StringElem("2 <= 2? true")),
          BasicElemPool(StringElem("2 >= 2? true")),
          BasicElemPool(StringElem("3 > 2 && 2 > 3? false")),
          BasicElemPool(StringElem("3 > 2 || 2 > 3? true")),
          BasicElemPool(StringElem("!(3 == 2)? true")),
          BasicElemPool(StringElem("\n->Inc/Dec-rementation")),
          MethodRef("println", "(I)V", Class("java/io/PrintStream")),
          BasicElemPool(StringElem("\n->Control Structures")),
          BasicElemPool(StringElem("I get printed")),
          BasicElemPool(StringElem("I don't")),
          BasicElemPool(StringElem("I also don't")),
          BasicElemPool(StringElem("fooWhile Value: ")),
          BasicElemPool(StringElem("fooDoWhile Value: ")),
          BasicElemPool(StringElem("January")),
          BasicElemPool(StringElem("February")),
          BasicElemPool(StringElem("March")),
          BasicElemPool(StringElem("Some other month")),
          BasicElemPool(StringElem("Switch Case Result: ")),
          BasicElemPool(StringElem("maybe")),
          MethodRef("hashCode", "()I", Class("java/lang/String")),
          BasicElemPool(StringElem("yes")),
          MethodRef("equals", "(Ljava/lang/Object;)Z", Class("java/lang/String")),
          BasicElemPool(StringElem("no")),
          BasicElemPool(StringElem("You answered yes.")),
          BasicElemPool(StringElem("You answered no.")),
          BasicElemPool(StringElem("You answered maybe.")),
          BasicElemPool(StringElem("You answered ")),
          BasicElemPool(StringElem("A")),
          BasicElemPool(StringElem("B")),
          BasicElemPool(StringElem("123")),
          MethodRef("parseInt", "(Ljava/lang/String;)I", Class("java/lang/Integer")),
          MethodRef("toString", "(I)Ljava/lang/String;", Class("java/lang/Integer")),
          Class("CodeTest"),
          BasicElemPool(StringElem("<init>")),
          BasicElemPool(StringElem("()V")),
          BasicElemPool(StringElem("Code")),
          BasicElemPool(StringElem("LineNumberTable")),
          BasicElemPool(StringElem("method")),
          BasicElemPool(StringElem("StackMapTable")),
          Class("CodeTest"),
          Class("java/math/BigInteger"),
          Class("java/math/BigDecimal"),
          Class("java/lang/String"),
          Class("[I"),
          Class("[Ljava/lang/String;"),
          Class("[Z"),
          BasicElemPool(StringElem("SourceFile")),
          BasicElemPool(StringElem("CodeTest.java")),
          NameAndType("<init>", "()V"),
          Class("java/lang/System"),
          NameAndType("out", "Ljava/io/PrintStream;"),
          BasicElemPool(StringElem("Hello World!")),
          Class("java/io/PrintStream"),
          NameAndType("println", "(Ljava/lang/String;)V"),
          BasicElemPool(StringElem("Integer: 10 Double: 3.14 Boolean: true")),
          BasicElemPool(StringElem("Hello ")),
          NameAndType("print", "(Ljava/lang/String;)V"),
          BasicElemPool(StringElem("World")),
          BasicElemPool(StringElem("pi = %.5f")),
          BasicElemPool(StringElem("java/lang/Object")),
          BasicElemPool(StringElem("java/lang/Math")),
          Class("java/lang/Double"),
          NameAndType("valueOf", "(D)Ljava/lang/Double;"),
          NameAndType("printf", "(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;"),
          BasicElemPool(StringElem("java/math/BigInteger")),
          BasicElemPool(StringElem("42")),
          NameAndType("<init>", "(Ljava/lang/String;)V"),
          BasicElemPool(StringElem("java/math/BigDecimal")),
          NameAndType("<init>", "(Ljava/math/BigInteger;I)V"),
          BasicElemPool(StringElem("0.1")),
          BasicElemPool(StringElem("My String Is Here!")),
          BasicElemPool(StringElem("Printing on a new line?\nNo Problem!")),
          BasicElemPool(StringElem("Do you want to add a tab?	No Problem!")),
          BasicElemPool(StringElem("java/lang/String")),
          BasicElemPool(StringElem("Bob")),
          BasicElemPool(StringElem("John")),
          BasicElemPool(StringElem("Fred")),
          BasicElemPool(StringElem("Juan Pedro")),
          BasicElemPool(StringElem("java/lang/StringBuilder")),
          BasicElemPool(StringElem("intArray @ 0: ")),
          NameAndType("append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;"),
          NameAndType("append", "(I)Ljava/lang/StringBuilder;"),
          NameAndType("toString", "()Ljava/lang/String;"),
          BasicElemPool(StringElem("intArray @ 1: ")),
          BasicElemPool(StringElem("\n->Operators")),
          BasicElemPool(StringElem("1+2 = ")),
          BasicElemPool(StringElem("2-1 = ")),
          BasicElemPool(StringElem("2*1 = ")),
          BasicElemPool(StringElem("1/2 = ")),
          NameAndType("append", "(D)Ljava/lang/StringBuilder;"),
          BasicElemPool(StringElem("11%3 = 2")),
          BasicElemPool(StringElem("3 == 2? false")),
          BasicElemPool(StringElem("3 != 2? true")),
          BasicElemPool(StringElem("3 > 2? true")),
          BasicElemPool(StringElem("3 < 2? false")),
          BasicElemPool(StringElem("2 <= 2? true")),
          BasicElemPool(StringElem("2 >= 2? true")),
          BasicElemPool(StringElem("3 > 2 && 2 > 3? false")),
          BasicElemPool(StringElem("3 > 2 || 2 > 3? true")),
          BasicElemPool(StringElem("!(3 == 2)? true")),
          BasicElemPool(StringElem("\n->Inc/Dec-rementation")),
          NameAndType("println", "(I)V"),
          BasicElemPool(StringElem("\n->Control Structures")),
          BasicElemPool(StringElem("I get printed")),
          BasicElemPool(StringElem("I don't")),
          BasicElemPool(StringElem("I also don't")),
          BasicElemPool(StringElem("fooWhile Value: ")),
          BasicElemPool(StringElem("fooDoWhile Value: ")),
          BasicElemPool(StringElem("January")),
          BasicElemPool(StringElem("February")),
          BasicElemPool(StringElem("March")),
          BasicElemPool(StringElem("Some other month")),
          BasicElemPool(StringElem("Switch Case Result: ")),
          BasicElemPool(StringElem("maybe")),
          NameAndType("hashCode", "()I"),
          BasicElemPool(StringElem("yes")),
          NameAndType("equals", "(Ljava/lang/Object;)Z"),
          BasicElemPool(StringElem("no")),
          BasicElemPool(StringElem("You answered yes.")),
          BasicElemPool(StringElem("You answered no.")),
          BasicElemPool(StringElem("You answered maybe.")),
          BasicElemPool(StringElem("You answered ")),
          BasicElemPool(StringElem("A")),
          BasicElemPool(StringElem("B")),
          BasicElemPool(StringElem("123")),
          Class("java/lang/Integer"),
          NameAndType("parseInt", "(Ljava/lang/String;)I"),
          NameAndType("toString", "(I)Ljava/lang/String;"),
          BasicElemPool(StringElem("CodeTest")),
          BasicElemPool(StringElem("[I")),
          BasicElemPool(StringElem("[Ljava/lang/String;")),
          BasicElemPool(StringElem("[Z")),
          BasicElemPool(StringElem("java/lang/System")),
          BasicElemPool(StringElem("out")),
          BasicElemPool(StringElem("Ljava/io/PrintStream;")),
          BasicElemPool(StringElem("java/io/PrintStream")),
          BasicElemPool(StringElem("println")),
          BasicElemPool(StringElem("(Ljava/lang/String;)V")),
          BasicElemPool(StringElem("print")),
          BasicElemPool(StringElem("java/lang/Double")),
          BasicElemPool(StringElem("valueOf")),
          BasicElemPool(StringElem("(D)Ljava/lang/Double;")),
          BasicElemPool(StringElem("printf")),
          BasicElemPool(StringElem("(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;")),
          BasicElemPool(StringElem("(Ljava/math/BigInteger;I)V")),
          BasicElemPool(StringElem("append")),
          BasicElemPool(StringElem("(Ljava/lang/String;)Ljava/lang/StringBuilder;")),
          BasicElemPool(StringElem("(I)Ljava/lang/StringBuilder;")),
          BasicElemPool(StringElem("toString")),
          BasicElemPool(StringElem("()Ljava/lang/String;")),
          BasicElemPool(StringElem("(D)Ljava/lang/StringBuilder;")),
          BasicElemPool(StringElem("(I)V")),
          BasicElemPool(StringElem("hashCode")),
          BasicElemPool(StringElem("()I")),
          BasicElemPool(StringElem("equals")),
          BasicElemPool(StringElem("(Ljava/lang/Object;)Z")),
          BasicElemPool(StringElem("java/lang/Integer")),
          BasicElemPool(StringElem("parseInt")),
          BasicElemPool(StringElem("(Ljava/lang/String;)I")),
          BasicElemPool(StringElem("(I)Ljava/lang/String;"))
        )

        val expectedCode = ArrayBuffer(
          // output from the javap -c
          // it was manually run to verify the correctness of parser result
          //
          // all gotos and if-conditions were printed with absolute addresses by javap
          // but actually they are relative

          GetStatic(2),          //   0: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(3),                //   3: ldc           #3                  // String Hello World!
          InvokeVirtual(4),      //   5: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          //   8: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(5),                //  11: ldc           #5                  // String Integer: 10 Double: 3.14 Boolean: true
          InvokeVirtual(4),      //  13: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          //  16: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(6),                //  19: ldc           #6                  // String Hello
          InvokeVirtual(7),      //  21: invokevirtual #7                  // Method java/io/PrintStream.print:(Ljava/lang/String;)V
          GetStatic(2),          //  24: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(8),                //  27: ldc           #8                  // String World
          InvokeVirtual(7),      //  29: invokevirtual #7                  // Method java/io/PrintStream.print:(Ljava/lang/String;)V
          GetStatic(2),          //  32: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(9),                //  35: ldc           #9                  // String pi = %.5f
          IConst1,               //  37: iconst_1
          ANewArray(10),         //  38: anewarray     #10                 // class java/lang/Object
          Dup,                   //  41: dup
          IConst0,               //  42: iconst_0
          LDC2W(12),             //  43: ldc2_w        #12                 // double 3.141592653589793d
          InvokeStatic(14),      //  46: invokestatic  #14                 // Method java/lang/Double.valueOf:(D)Ljava/lang/Double;
          AAStore,               //  49: aastore
          InvokeVirtual(15),     //  50: invokevirtual #15                 // Method java/io/PrintStream.printf:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;
          Pop,                   //  53: pop
          IConst1,               //  54: iconst_1
          IStore(4),             //  55: istore        4
          IConst1,               //  57: iconst_1
          Dup,                   //  58: dup
          IStore3,               //  59: istore_3
          Dup,                   //  60: dup
          IStore2,               //  61: istore_2
          IStore1,               //  62: istore_1
          BIPush(100),           //  63: bipush        100
          IStore(5),             //  65: istore        5
          SIPush(10000),         //  67: sipush        10000
          IStore(6),             //  70: istore        6
          LDC2W(16),             //  72: ldc2_w        #16                 // long 100000l
          LStore(7),             //  75: lstore        7
          LDC(18),               //  77: ldc           #18                 // float 234.5f
          FStore(9),             //  79: fstore        9
          LDC2W(19),             //  81: ldc2_w        #19                 // double 123.4d
          DStore(10),            //  84: dstore        10
          IConst1,               //  86: iconst_1
          IStore(12),            //  87: istore        12
          IConst0,               //  89: iconst_0
          IStore(13),            //  90: istore        13
          BIPush(65),            //  92: bipush        65
          IStore(14),            //  94: istore        14
          LDC2W(21),             //  96: ldc2_w        #21                 // double 2.71828d
          DStore(16),            //  99: dstore        16
          New(23),               // 101: new           #23                 // class java/math/BigInteger
          Dup,                   // 104: dup
          LDC(24),               // 105: ldc           #24                 // String 42
          InvokeSpecial(25),     // 107: invokespecial #25                 // Method java/math/BigInteger."<init>":(Ljava/lang/String;)V
          AStore(18),            // 110: astore        18
          New(26),               // 112: new           #26                 // class java/math/BigDecimal
          Dup,                   // 115: dup
          ALoad(18),             // 116: aload         18
          ILoad(4),              // 118: iload         4
          InvokeSpecial(27),     // 120: invokespecial #27                 // Method java/math/BigDecimal."<init>":(Ljava/math/BigInteger;I)V
          AStore(19),            // 123: astore        19
          New(26),               // 125: new           #26                 // class java/math/BigDecimal
          Dup,                   // 128: dup
          LDC(28),               // 129: ldc           #28                 // String 0.1
          InvokeSpecial(29),     // 131: invokespecial #29                 // Method java/math/BigDecimal."<init>":(Ljava/lang/String;)V
          AStore(20),            // 134: astore        20
          LDC(30),               // 136: ldc           #30                 // String My String Is Here!
          AStore(21),            // 138: astore        21
          LDC(31),               // 140: ldc           #31                 // String Printing on a new line?\nNo Problem!
          AStore(22),            // 142: astore        22
          LDC(32),               // 144: ldc           #32                 // String Do you want to add a tab?\tNo Problem!
          AStore(23),            // 146: astore        23
          GetStatic(2),          // 148: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ALoad(21),             // 151: aload         21
          InvokeVirtual(4),      // 153: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 156: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ALoad(22),             // 159: aload         22
          InvokeVirtual(4),      // 161: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 164: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ALoad(23),             // 167: aload         23
          InvokeVirtual(4),      // 169: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          BIPush(10),            // 172: bipush        10
          NewArray(10),          // 174: newarray       int
          AStore(24),            // 176: astore        24
          IConst1,               // 178: iconst_1
          ANewArray(33),         // 179: anewarray     #33                 // class java/lang/String
          AStore(25),            // 182: astore        25
          BIPush(100),           // 184: bipush        100
          NewArray(4),           // 186: newarray       boolean
          AStore(26),            // 188: astore        26
          IConst3,               // 190: iconst_3
          NewArray(10),          // 191: newarray       int
          Dup,                   // 193: dup
          IConst0,               // 194: iconst_0
          SIPush(9000),          // 195: sipush        9000
          IAStore,               // 198: iastore
          Dup,                   // 199: dup
          IConst1,               // 200: iconst_1
          SIPush(1000),          // 201: sipush        1000
          IAStore,               // 204: iastore
          Dup,                   // 205: dup
          IConst2,               // 206: iconst_2
          SIPush(1337),          // 207: sipush        1337
          IAStore,               // 210: iastore
          AStore(27),            // 211: astore        27
          IConst4,               // 213: iconst_4
          ANewArray(33),         // 214: anewarray     #33                 // class java/lang/String
          Dup,                   // 217: dup
          IConst0,               // 218: iconst_0
          LDC(34),               // 219: ldc           #34                 // String Bob
          AAStore,               // 221: aastore
          Dup,                   // 222: dup
          IConst1,               // 223: iconst_1
          LDC(35),               // 224: ldc           #35                 // String John
          AAStore,               // 226: aastore
          Dup,                   // 227: dup
          IConst2,               // 228: iconst_2
          LDC(36),               // 229: ldc           #36                 // String Fred
          AAStore,               // 231: aastore
          Dup,                   // 232: dup
          IConst3,               // 233: iconst_3
          LDC(37),               // 234: ldc           #37                 // String Juan Pedro
          AAStore,               // 236: aastore
          AStore(28),            // 237: astore        28
          IConst3,               // 239: iconst_3
          NewArray(4),           // 240: newarray       boolean
          Dup,                   // 242: dup
          IConst0,               // 243: iconst_0
          IConst1,               // 244: iconst_1
          BAStore,               // 245: bastore
          Dup,                   // 246: dup
          IConst1,               // 247: iconst_1
          IConst0,               // 248: iconst_0
          BAStore,               // 249: bastore
          Dup,                   // 250: dup
          IConst2,               // 251: iconst_2
          IConst0,               // 252: iconst_0
          BAStore,               // 253: bastore
          AStore(29),            // 254: astore        29
          GetStatic(2),          // 256: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 259: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 262: dup
          InvokeSpecial(39),     // 263: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(40),               // 266: ldc           #40                 // String intArray @ 0:
          InvokeVirtual(41),     // 268: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ALoad(24),             // 271: aload         24
          IConst0,               // 273: iconst_0
          IALoad,                // 274: iaload
          InvokeVirtual(42),     // 275: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 278: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 281: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          ALoad(24),             // 284: aload         24
          IConst1,               // 286: iconst_1
          IConst1,               // 287: iconst_1
          IAStore,               // 288: iastore
          GetStatic(2),          // 289: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 292: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 295: dup
          InvokeSpecial(39),     // 296: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(44),               // 299: ldc           #44                 // String intArray @ 1:
          InvokeVirtual(41),     // 301: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ALoad(24),             // 304: aload         24
          IConst1,               // 306: iconst_1
          IALoad,                // 307: iaload
          InvokeVirtual(42),     // 308: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 311: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 314: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 317: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(45),               // 320: ldc           #45                 // String \n->Operators
          InvokeVirtual(4),      // 322: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst1,               // 325: iconst_1
          IStore(30),            // 326: istore        30
          IConst2,               // 328: iconst_2
          IStore(31),            // 329: istore        31
          GetStatic(2),          // 331: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 334: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 337: dup
          InvokeSpecial(39),     // 338: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(46),               // 341: ldc           #46                 // String 1+2 =
          InvokeVirtual(41),     // 343: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(30),             // 346: iload         30
          ILoad(31),             // 348: iload         31
          IAdd,                  // 350: iadd
          InvokeVirtual(42),     // 351: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 354: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 357: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 360: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 363: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 366: dup
          InvokeSpecial(39),     // 367: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(47),               // 370: ldc           #47                 // String 2-1 =
          InvokeVirtual(41),     // 372: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(31),             // 375: iload         31
          ILoad(30),             // 377: iload         30
          ISub,                  // 379: isub
          InvokeVirtual(42),     // 380: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 383: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 386: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 389: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 392: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 395: dup
          InvokeSpecial(39),     // 396: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(48),               // 399: ldc           #48                 // String 2*1 =
          InvokeVirtual(41),     // 401: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(31),             // 404: iload         31
          ILoad(30),             // 406: iload         30
          IMul,                  // 408: imul
          InvokeVirtual(42),     // 409: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 412: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 415: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 418: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 421: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 424: dup
          InvokeSpecial(39),     // 425: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(49),               // 428: ldc           #49                 // String 1/2 =
          InvokeVirtual(41),     // 430: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(30),             // 433: iload         30
          ILoad(31),             // 435: iload         31
          IDiv,                  // 437: idiv
          InvokeVirtual(42),     // 438: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 441: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 444: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 447: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 450: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 453: dup
          InvokeSpecial(39),     // 454: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(49),               // 457: ldc           #49                 // String 1/2 =
          InvokeVirtual(41),     // 459: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(30),             // 462: iload         30
          I2D,                   // 464: i2d
          ILoad(31),             // 465: iload         31
          I2D,                   // 467: i2d
          DDiv,                  // 468: ddiv
          InvokeVirtual(50),     // 469: invokevirtual #50                 // Method java/lang/StringBuilder.append:(D)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 472: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 475: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 478: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(51),               // 481: ldc           #51                 // String 11%3 = 2
          InvokeVirtual(4),      // 483: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 486: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(52),               // 489: ldc           #52                 // String 3 == 2? false
          InvokeVirtual(4),      // 491: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 494: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(53),               // 497: ldc           #53                 // String 3 != 2? true
          InvokeVirtual(4),      // 499: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 502: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(54),               // 505: ldc           #54                 // String 3 > 2? true
          InvokeVirtual(4),      // 507: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 510: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(55),               // 513: ldc           #55                 // String 3 < 2? false
          InvokeVirtual(4),      // 515: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 518: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(56),               // 521: ldc           #56                 // String 2 <= 2? true
          InvokeVirtual(4),      // 523: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 526: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(57),               // 529: ldc           #57                 // String 2 >= 2? true
          InvokeVirtual(4),      // 531: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 534: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(58),               // 537: ldc           #58                 // String 3 > 2 && 2 > 3? false
          InvokeVirtual(4),      // 539: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 542: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(59),               // 545: ldc           #59                 // String 3 > 2 || 2 > 3? true
          InvokeVirtual(4),      // 547: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 550: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(60),               // 553: ldc           #60                 // String !(3 == 2)? true
          InvokeVirtual(4),      // 555: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst0,               // 558: iconst_0
          IStore(32),            // 559: istore        32
          GetStatic(2),          // 561: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(61),               // 564: ldc           #61                 // String \n->Inc/Dec-rementation
          InvokeVirtual(4),      // 566: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          GetStatic(2),          // 569: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(32),             // 572: iload         32
          IInc(32,1),            // 574: iinc          32, 1
          InvokeVirtual(62),     // 577: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          GetStatic(2),          // 580: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          IInc(32,1),            // 583: iinc          32, 1
          ILoad(32),             // 586: iload         32
          InvokeVirtual(62),     // 588: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          GetStatic(2),          // 591: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(32),             // 594: iload         32
          IInc(32,-1),           // 596: iinc          32, -1
          InvokeVirtual(62),     // 599: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          GetStatic(2),          // 602: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          IInc(32,-1),           // 605: iinc          32, -1
          ILoad(32),             // 608: iload         32
          InvokeVirtual(62),     // 610: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          GetStatic(2),          // 613: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(63),               // 616: ldc           #63                 // String \n->Control Structures
          InvokeVirtual(4),      // 618: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          BIPush(10),            // 621: bipush        10
          IStore(33),            // 623: istore        33
          ILoad(33),             // 625: iload         33
          BIPush(10),            // 627: bipush        10
          IfICmpNe(14),          // 629: if_icmpne     643
          GetStatic(2),          // 632: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(64),               // 635: ldc           #64                 // String I get printed
          InvokeVirtual(4),      // 637: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          Goto(29),              // 640: goto          669
          ILoad(33),             // 643: iload         33
          BIPush(10),            // 645: bipush        10
          IfICmpLe(14),          // 647: if_icmple     661
          GetStatic(2),          // 650: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(65),               // 653: ldc           #65                 // String I don't
          InvokeVirtual(4),      // 655: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          Goto(11),              // 658: goto          669
          GetStatic(2),          // 661: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(66),               // 664: ldc           #66                 // String I also don't
          InvokeVirtual(4),      // 666: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst0,               // 669: iconst_0
          IStore(34),            // 670: istore        34
          ILoad(34),             // 672: iload         34
          BIPush(100),           // 674: bipush        100
          IfICmpGe(17),          // 676: if_icmpge     693
          GetStatic(2),          // 679: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(34),             // 682: iload         34
          InvokeVirtual(62),     // 684: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          IInc(34,1),            // 687: iinc          34, 1
          Goto(-18),             // 690: goto          672
          GetStatic(2),          // 693: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 696: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 699: dup
          InvokeSpecial(39),     // 700: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(67),               // 703: ldc           #67                 // String fooWhile Value:
          InvokeVirtual(41),     // 705: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(34),             // 708: iload         34
          InvokeVirtual(42),     // 710: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 713: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 716: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst0,               // 719: iconst_0
          IStore(35),            // 720: istore        35
          GetStatic(2),          // 722: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(35),             // 725: iload         35
          InvokeVirtual(62),     // 727: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          IInc(35,1),            // 730: iinc          35, 1
          ILoad(35),             // 733: iload         35
          BIPush(100),           // 735: bipush        100
          IfICmpLt(-15),         // 737: if_icmplt     722
          GetStatic(2),          // 740: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 743: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 746: dup
          InvokeSpecial(39),     // 747: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(68),               // 750: ldc           #68                 // String fooDoWhile Value:
          InvokeVirtual(41),     // 752: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ILoad(35),             // 755: iload         35
          InvokeVirtual(42),     // 757: invokevirtual #42                 // Method java/lang/StringBuilder.append:(I)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     // 760: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      // 763: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst0,               // 766: iconst_0
          IStore(36),            // 767: istore        36
          ILoad(36),             // 769: iload         36
          BIPush(10),            // 771: bipush        10
          IfICmpGe(17),          // 773: if_icmpge     790
          GetStatic(2),          // 776: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(36),             // 779: iload         36
          InvokeVirtual(62),     // 781: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          IInc(36,1),            // 784: iinc          36, 1
          Goto(-18),             // 787: goto          769
          IConst0,               // 790: iconst_0
          IStore(36),            // 791: istore        36
          ILoad(36),             // 793: iload         36
          BIPush(10),            // 795: bipush        10
          IfICmpGe(40),          // 797: if_icmpge     837
          IConst0,               // 800: iconst_0
          IStore(37),            // 801: istore        37
          ILoad(37),             // 803: iload         37
          BIPush(10),            // 805: bipush        10
          IfICmpGe(24),          // 807: if_icmpge     831
          ILoad(36),             // 810: iload         36
          IConst5,               // 812: iconst_5
          IfICmpNe(12),          // 813: if_icmpne     825
          ILoad(37),             // 816: iload         37
          IConst5,               // 818: iconst_5
          IfICmpNe(6),           // 819: if_icmpne     825
          Goto(15),              // 822: goto          837
          IInc(37,1),            // 825: iinc          37, 1
          Goto(-25),             // 828: goto          803
          IInc(36,1),            // 831: iinc          36, 1
          Goto(-41),             // 834: goto          793
          BIPush(9),             // 837: bipush        9
          NewArray(10),          // 839: newarray       int
          Dup,                   // 841: dup
          IConst0,               // 842: iconst_0
          IConst1,               // 843: iconst_1
          IAStore,               // 844: iastore
          Dup,                   // 845: dup
          IConst1,               // 846: iconst_1
          IConst2,               // 847: iconst_2
          IAStore,               // 848: iastore
          Dup,                   // 849: dup
          IConst2,               // 850: iconst_2
          IConst3,               // 851: iconst_3
          IAStore,               // 852: iastore
          Dup,                   // 853: dup
          IConst3,               // 854: iconst_3
          IConst4,               // 855: iconst_4
          IAStore,               // 856: iastore
          Dup,                   // 857: dup
          IConst4,               // 858: iconst_4
          IConst5,               // 859: iconst_5
          IAStore,               // 860: iastore
          Dup,                   // 861: dup
          IConst5,               // 862: iconst_5
          BIPush(6),             // 863: bipush        6
          IAStore,               // 865: iastore
          Dup,                   // 866: dup
          BIPush(6),             // 867: bipush        6
          BIPush(7),             // 869: bipush        7
          IAStore,               // 871: iastore
          Dup,                   // 872: dup
          BIPush(7),             // 873: bipush        7
          BIPush(8),             // 875: bipush        8
          IAStore,               // 877: iastore
          Dup,                   // 878: dup
          BIPush(8),             // 879: bipush        8
          BIPush(9),             // 881: bipush        9
          IAStore,               // 883: iastore
          AStore(36),            // 884: astore        36
          ALoad(36),             // 886: aload         36
          AStore(37),            // 888: astore        37
          ALoad(37),             // 890: aload         37
          ArrayLength,           // 892: arraylength
          IStore(38),            // 893: istore        38
          IConst0,               // 895: iconst_0
          IStore(39),            // 896: istore        39
          ILoad(39),             // 898: iload         39
          ILoad(38),             // 900: iload         38
          IfICmpGe(24),          // 902: if_icmpge     926
          ALoad(37),             // 905: aload         37
          ILoad(39),             // 907: iload         39
          IALoad,                // 909: iaload
          IStore(40),            // 910: istore        40
          GetStatic(2),          // 912: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ILoad(40),             // 915: iload         40
          InvokeVirtual(62),     // 917: invokevirtual #62                 // Method java/io/PrintStream.println:(I)V
          IInc(39,1),            // 920: iinc          39, 1
          Goto(-25),             // 923: goto          898
          IConst3,               // 926: iconst_3
          IStore(37),            // 927: istore        37
          ILoad(37),             // 929: iload         37
          TableSwitch(46, 1, 3, ArrayBuffer(25, 32, 39)),   //931: tableswitch   { 1: 956 2: 963 3: 970 default: 977 }
          LDC(69),               // 956: ldc           #69                 // String January
          AStore(38),            // 958: astore        38
          Goto(21),              // 960: goto          981
          LDC(70),               // 963: ldc           #70                 // String February
          AStore(38),            // 965: astore        38
          Goto(14),              // 967: goto          981
          LDC(71),               // 970: ldc           #71                 // String March
          AStore(38),            // 972: astore        38
          Goto(7),               // 974: goto          981
          LDC(72),               // 977: ldc           #72                 // String Some other month
          AStore(38),            // 979: astore        38
          GetStatic(2),          // 981: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               // 984: new           #38                 // class java/lang/StringBuilder
          Dup,                   // 987: dup
          InvokeSpecial(39),     // 988: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(73),               // 991: ldc           #73                 // String Switch Case Result:
          InvokeVirtual(41),     // 993: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ALoad(38),             // 996: aload         38
          InvokeVirtual(41),     // 998: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     //1001: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      //1004: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          LDC(74),               //1007: ldc           #74                 // String maybe
          AStore(39),            //1009: astore        39
          ALoad(39),             //1011: aload         39
          AStore(40),            //1013: astore        40
          IConstM1,              //1015: iconst_m1
          IStore(41),            //1016: istore        41
          ALoad(40),             //1018: aload         40
          InvokeVirtual(75),     //1020: invokevirtual #75                 // Method java/lang/String.hashCode:()I
          LookUpSwitch(78, ArrayBuffer((3521,49), (119527,33), (103672936,65))),   //1023: lookupswitch  { 3521: 1072 119527: 1056 103672936: 1088 default: 1101 }
          ALoad(40),             //1056: aload         40
          LDC(76),               //1058: ldc           #76                 // String yes
          InvokeVirtual(77),     //1060: invokevirtual #77                 // Method java/lang/String.equals:(Ljava/lang/Object;)Z
          IfEq(38),              //1063: ifeq          1101
          IConst0,               //1066: iconst_0
          IStore(41),            //1067: istore        41
          Goto(32),              //1069: goto          1101
          ALoad(40),             //1072: aload         40
          LDC(78),               //1074: ldc           #78                 // String no
          InvokeVirtual(77),     //1076: invokevirtual #77                 // Method java/lang/String.equals:(Ljava/lang/Object;)Z
          IfEq(22),              //1079: ifeq          1101
          IConst1,               //1082: iconst_1
          IStore(41),            //1083: istore        41
          Goto(16),              //1085: goto          1101
          ALoad(40),             //1088: aload         40
          LDC(74),               //1090: ldc           #74                 // String maybe
          InvokeVirtual(77),     //1092: invokevirtual #77                 // Method java/lang/String.equals:(Ljava/lang/Object;)Z
          IfEq(6),               //1095: ifeq          1101
          IConst2,               //1098: iconst_2
          IStore(41),            //1099: istore        41
          ILoad(41),             //1101: iload         41
          TableSwitch(58, 0, 2, ArrayBuffer(25, 36, 47)),    //1103: tableswitch   { 0: 1128 1: 1139 2: 1150 default: 1161 }
          GetStatic(2),          //1128: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(79),               //1131: ldc           #79                 // String You answered yes.
          InvokeVirtual(4),      //1133: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          Goto(51),              //1136: goto          1187
          GetStatic(2),          //1139: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(80),               //1142: ldc           #80                 // String You answered no.
          InvokeVirtual(4),      //1144: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          Goto(40),              //1147: goto          1187
          GetStatic(2),          //1150: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          LDC(81),               //1153: ldc           #81                 // String You answered maybe.
          InvokeVirtual(4),      //1155: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          Goto(29),              //1158: goto          1187
          GetStatic(2),          //1161: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          New(38),               //1164: new           #38                 // class java/lang/StringBuilder
          Dup,                   //1167: dup
          InvokeSpecial(39),     //1168: invokespecial #39                 // Method java/lang/StringBuilder."<init>":()V
          LDC(82),               //1171: ldc           #82                 // String You answered
          InvokeVirtual(41),     //1173: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          ALoad(39),             //1176: aload         39
          InvokeVirtual(41),     //1178: invokevirtual #41                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          InvokeVirtual(43),     //1181: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          InvokeVirtual(4),      //1184: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          IConst5,               //1187: iconst_5
          IStore(40),            //1188: istore        40
          ILoad(40),             //1190: iload         40
          BIPush(10),            //1192: bipush        10
          IfICmpGe(8),           //1194: if_icmpge     1202
          LDC(83),               //1197: ldc           #83                 // String A
          Goto(5),               //1199: goto          1204
          LDC(84),               //1202: ldc           #84                 // String B
          AStore(41),            //1204: astore        41
          GetStatic(2),          //1206: getstatic     #2                  // Field java/lang/System.out:Ljava/io/PrintStream;
          ALoad(41),             //1209: aload         41
          InvokeVirtual(4),      //1211: invokevirtual #4                  // Method java/io/PrintStream.println:(Ljava/lang/String;)V
          LDC(85),               //1214: ldc           #85                 // String 123
          InvokeStatic(86),      //1216: invokestatic  #86                 // Method java/lang/Integer.parseInt:(Ljava/lang/String;)I
          Pop,                   //1219: pop
          BIPush(123),           //1220: bipush        123
          InvokeStatic(87),      //1222: invokestatic  #87                 // Method java/lang/Integer.toString:(I)Ljava/lang/String;
          Pop,                   //1225: pop
          Return                 //1226: return
        )

        val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parse(classFile)
        val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

        assert(parsedClass.pool == expectedPool)

        assert(
          parsedClass.methods(1).attributes(0).asInstanceOf[CodeAttribute].code ==
          expectedCode
        )

        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val Parsed.Success(parsedClassInfo, _) = ClassParse.classFile.parseIterator(
            classFile.toArray.grouped(chunkSize).map(Bytes.view)
          )
          val parsedClass = ClassParse.Ast.convertToAst(parsedClassInfo)

          assert(parsedClass.pool == expectedPool)

          assert(
            parsedClass.methods(1).attributes(0).asInstanceOf[CodeAttribute].code ==
              expectedCode
          )
        }
      }
    }
  }
}