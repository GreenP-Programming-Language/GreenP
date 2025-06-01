
        function main(): void {
            printNumber(1);
            const x: number = 10;
            let y: number = x + 5; // y = 15
            printNumber(y);      // Saída: 15
            y = y - x;           // y = 15 - 10 = 5
            printNumber(y);      // Saída: 5
        }
        