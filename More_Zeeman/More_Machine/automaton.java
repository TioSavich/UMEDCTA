import java.util.Scanner;

public class Automaton {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int[][] matrix = {{1}};
        int size = 1;

        while (true) {
            System.out.println("Current Matrix:");
            printMatrix(matrix, size);

            System.out.print("Enter 0 or 1: ");
            int input = scanner.nextInt();

            matrix = nextIteration(matrix, size, input);
            size++;
        }
    }

    private static int[][] nextIteration(int[][] matrix, int size, int input) {
        int newSize = size + 1;
        int[][] newMatrix = new int[newSize][newSize];

        // Copy existing matrix
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                newMatrix[i][j] = matrix[i][j];
            }
        }

        // Negate diagonal
        for (int i = 0; i < size; i++) {
            newMatrix[i][i] = 1 - newMatrix[i][i];
        }

        // Place the user's input in the new diagonal position
        newMatrix[size][size] = input;

        return newMatrix;
    }

    private static void printMatrix(int[][] matrix, int size) {
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                System.out.print(matrix[i][j] + " ");
            }
            System.out.println();
        }
        System.out.println();
    }
}
// JavaScript Document