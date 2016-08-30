// The code was taken from https://learnxinyminutes.com/docs/java/

import java.math.BigInteger;
import java.math.BigDecimal;

public class CodeTest {
    public void method() {
        System.out.println("Hello World!");
        System.out.println("Integer: " + 10 + " Double: " + 3.14 + " Boolean: " + true);
        System.out.print("Hello ");
        System.out.print("World");        
        System.out.printf("pi = %.5f", Math.PI);

        int fooInt1, fooInt2, fooInt3;
        int fooInt = 1;

        fooInt1 = fooInt2 = fooInt3 = 1;

        byte fooByte = 100;
        short fooShort = 10000;
        long fooLong = 100000L;
        float fooFloat = 234.5f;
        double fooDouble = 123.4;
        boolean fooBoolean = true;
        boolean barBoolean = false;        
        char fooChar = 'A';        
        final int HOURS_I_WORK_PER_WEEK = 9001;
        final double E;
        E = 2.71828;
        BigInteger fooBigInteger = new BigInteger("42");
        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, fooInt);
        BigDecimal tenCents = new BigDecimal("0.1");        
        String fooString = "My String Is Here!";        
        String barString = "Printing on a new line?\nNo Problem!";
        String bazString = "Do you want to add a tab?\tNo Problem!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];        
        int[] y = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = {true, false, false};        
        System.out.println("intArray @ 0: " + intArray[0]);        
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]);
        System.out.println("\n->Operators");        int i1 = 1, i2 = 2;         
        System.out.println("1+2 = " + (i1 + i2)); 
        System.out.println("2-1 = " + (i2 - i1)); 
        System.out.println("2*1 = " + (i2 * i1)); 
        System.out.println("1/2 = " + (i1 / i2)); 
        System.out.println("1/2 = " + (i1 / (double)i2));         
        System.out.println("11%3 = "+(11 % 3));         
        System.out.println("3 == 2? " + (3 == 2)); 
        System.out.println("3 != 2? " + (3 != 2)); 
        System.out.println("3 > 2? " + (3 > 2)); 
        System.out.println("3 < 2? " + (3 < 2)); 
        System.out.println("2 <= 2? " + (2 <= 2)); 
        System.out.println("2 >= 2? " + (2 >= 2));         
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); 
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); 
        System.out.println("!(3 == 2)? " + (!(3 == 2)));
        int i = 0;
        System.out.println("\n->Inc/Dec-rementation");
        System.out.println(i++); 
        System.out.println(++i); 
        System.out.println(i--); 
        System.out.println(--i);
        System.out.println("\n->Control Structures");        
        int j = 10;
        if (j == 10) {
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }        
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);        
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
        }
        outer:
        for (int ii = 0; ii < 10; ii++) {
            for (int jj = 0; jj < 10; jj++) {
                if (ii == 5 && jj ==5) {
                    break outer;
                }
            }
        }
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        for (int bar : fooList) {
            System.out.println(bar);
        }
        int month = 3;
        String monthString;
        switch (month) {
            case 1: monthString = "January";
                break;
            case 2: monthString = "February";
                break;
            case 3: monthString = "March";
                break;
            default: monthString = "Some other month";
                break;
        }
        System.out.println("Switch Case Result: " + monthString);        
        String myAnswer = "maybe";
        switch(myAnswer) {
            case "yes":
                System.out.println("You answered yes.");
                break;
            case "no":
                System.out.println("You answered no.");
                break;
            case "maybe":
                System.out.println("You answered maybe.");
                break;
            default:
                System.out.println("You answered " + myAnswer);
                break;
        }
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar);
        Integer.parseInt("123");        
        Integer.toString(123);
    }
}