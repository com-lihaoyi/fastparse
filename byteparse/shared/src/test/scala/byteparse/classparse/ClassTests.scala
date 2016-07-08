package byteparse
package classparse

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
        /* Compiled class Book.java

        class Book {
            private String title;
            private int pubYear;

            public String getTitle() {
                return title;
            }

            public int getPubYear() {
                return pubYear;
            }

            public void setTitle(String title) {
                this.title = title;
            }

            public void setPubYear(int pubYear) {
                this.pubYear = pubYear;
            }
        }
        */

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
    }
  }
}
