function main(): void {
  const message: string = "Olá do GreenP, compilado e funcionando!";
  printString(message);

  let ano: number = 2025;
  printNumber(ano);
  
  const proximoAno: number = ano + 1;
  printString("Próximo ano:");
  printNumber(proximoAno);
}