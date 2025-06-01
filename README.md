## Licença

O projeto GreenP é duplamente licenciado sob os termos da **Licença MIT** E da **Licença Apache, Versão 2.0**.

Isso significa que você pode escolher utilizar o software GreenP sob os termos de qualquer uma dessas licenças, à sua discrição.

* Para os termos completos da Licença MIT, consulte o arquivo [LICENSE-MIT](LICENSE-MIT).
* Para os termos completos da Licença Apache 2.0, consulte o arquivo [LICENSE-APACHE](LICENSE-APACHE).

Ao contribuir para o projeto GreenP, você concorda que suas contribuições serão licenciadas da mesma forma.


# Bem-vindo à GreenP: Programando um Futuro Sustentável

## Visão Geral

**GreenP** é uma linguagem de programação inovadora, compilada e de alto desempenho, projetada desde sua fundação com um objetivo primordial: **reduzir drasticamente o consumo de energia do software global**. Inspirada na segurança e performance de Rust, mas com um foco implacável na eficiência energética, GreenP visa capacitar desenvolvedores a construir aplicações robustas e rápidas que sejam fundamentalmente mais leves para o planeta.

Estamos construindo a GreenP para ser uma ferramenta poderosa no combate à crescente pegada de carbono da computação, com o potencial de economizar uma quantidade significativa de energia anualmente (estudos iniciais apontam para uma redução na ordem de dezenas de Terawatts-hora/ano) se amplamente adotada.

## exemplo de um código em greenp:

function main(): void {
  const message: string = "Olá do GreenP, compilado e funcionando!";
  printString(message);
  
  let ano: number = 2025;
  printNumber(ano);
  
  const proximoAno: number = ano + 1;
  printString("Próximo ano:");
  printNumber(proximoAno);
}

## O Desafio: A Pegada Energética do Software

O software moderno, apesar de seus imensos benefícios, consome uma quantidade cada vez maior de energia elétrica. Desde os data centers que alimentam a nuvem até os bilhões de dispositivos embarcados e móveis, a eficiência do código que executamos tem um impacto direto e cumulativo no consumo energético global e nas emissões de carbono. Muitas linguagens e práticas de desenvolvimento atuais não foram criadas com a eficiência energética como uma preocupação de primeira classe.

## A Solução GreenP: Eficiência Energética por Design

GreenP aborda esse desafio integrando a eficiência energética em seu núcleo, através de:

1.  **Design da Linguagem Consciente:** Construções de linguagem que promovem baixo overhead e controle explícito sobre recursos críticos para o consumo de energia.
2.  **Compilador Altamente Otimizado:** Um compilador Ahead-of-Time (AOT) focado não apenas em velocidade, mas em gerar o código de máquina mais eficiente energeticamente para diversas arquiteturas (x86, ARM, RISC-V), potencialmente utilizando LLVM como backend e incorporando passes de otimização "verdes" específicos.
3.  **Gerenciamento de Memória Seguro e Eficiente:** Assim como Rust, GreenP utilizará um sistema de **propriedade e empréstimo (ownership & borrowing)** e o padrão **RAII (Resource Acquisition Is Initialization)**, eliminando a necessidade de um garbage collector (coletor de lixo) e suas pausas e overhead energético associados.
4.  **Biblioteca Padrão Enxuta e Otimizada:** Uma biblioteca padrão que oferece funcionalidades essenciais implementadas com máxima eficiência energética.
5.  **Promoção de Padrões de Código "Verdes":** Incentivar e facilitar, através da linguagem e ferramentas, práticas de desenvolvimento que resultem em menor consumo de energia.

## Como a GreenP Funciona e Reduz o Consumo de Energia

A estratégia da GreenP para a sustentabilidade energética é multifacetada, permeando desde o design da linguagem até a execução do código compilado.

### Modelo Conceitual de Eficiência GreenP

A energia consumida por um programa ($E_{total}$) pode ser simplificada como:

$E_{total} = Potência \times Tempo$

GreenP visa minimizar ambos os componentes através de:

* **Redução do Tempo de Execução:**
    * Compilação AOT para código nativo otimizado.
    * Abstrações de custo zero (zero-cost abstractions).
    * Estruturas de dados e algoritmos eficientes na biblioteca padrão.
* **Redução da Potência Consumida (por operação ou durante a execução):**
    * **Menos Instruções e Acessos à Memória:** Otimizações de compilador (e.g., loop unrolling, vetorização SIMD, inlining agressivo guiado por PGO-energético) e um design de linguagem que evita overhead desnecessário buscam reduzir o número total de instruções da CPU (`Ninstr`) e acessos à memória (`Nmem`).
    * **Gerenciamento de Memória Direto:** A ausência de um garbage collector elimina o consumo de energia associado aos ciclos de coleta (`Eruntime_overhead` significativamente reduzido).
    * **Controle Fino sobre Concorrência:** Tarefas leves (green tasks/fibers) e comunicação via canais minimizam o overhead de threads do sistema operacional.
    * **Layout de Dados Otimizado:** Controle sobre o layout de `structs` (e.g., `packed struct`) para melhor localidade de cache e menor consumo em acessos à memória.

**(Sugestão de Diagrama 1: Fluxo de Compilação GreenP)**

* **Descrição:** Um fluxograma mostrando as etapas:
    1.  Código Fonte GreenP (`.gp`)
    2.  Frontend do Compilador GreenP (Análise Léxica, Sintática, Semântica - incluindo checagens de propriedade e tempo de vida)
    3.  Geração de Representação Intermediária (IR) (e.g., LLVM IR)
    4.  Passes de Otimização GreenP (incluindo Otimizações Clássicas + **Otimizações Específicas de Energia**)
    5.  Backend LLVM (Otimizações de Baixo Nível e Geração de Código de Máquina)
    6.  Executável Nativo Eficiente

**(Sugestão de Diagrama 2: Pilares da Eficiência Energética da GreenP)**

* **Descrição:** Um diagrama com GreenP no centro e setas apontando para seus pilares de eficiência:
    * **Design da Linguagem:** (Sintaxe de baixo overhead, Imutabilidade por padrão, Tipos de dados com controle de layout, Concorrência leve)
    * **Compilador Otimizado:** (AOT, LLVM, Energy-PGO, Vetorização, DCE)
    * **Gerenciamento de Memória:** (Ownership & Borrowing, RAII, Sem GC)
    * **Biblioteca Padrão Eficiente:** (Coleções otimizadas, I/O assíncrona, APIs "verdes")
    * **Práticas de Codificação:** (Incentivadas pela linguagem e ferramentas)
    * *Resultado Comum:* Redução de $E_{total}$ (Menos `Ninstr`, `Nmem`, `Eruntime_overhead`)

### Estrutura da Linguagem e Recursos Chave

* **Sintaxe Inspirada em Rust:** Clareza, expressividade e segurança.
* **Tipagem Estática Forte:** Detecção de erros em tempo de compilação.
* **Segurança de Memória:** Garantida pelo sistema de propriedade e empréstimo, eliminando a necessidade de coletor de lixo.
    * `let mut variavel_mutavel = ...;`
    * `let variavel_imutavel = ...;`
    * Gerenciamento de tempo de vida similar ao Rust para evitar ponteiros pendentes.
* **Controle Explícito:** Palavras-chave como `packed struct` para controle fino sobre o layout de memória.
* **Funções e Modularidade:** Sistema de módulos para organização de código.
* **Tratamento de Erros:** Baseado em tipos `Result<T, E>` para tratamento explícito e robusto de falhas.
* **Concorrência:**
    * Tarefas leves (ex: `go { /* código concorrente */ }`)
    * Canais para comunicação segura entre tarefas (ex: `let ch = channel<i32>(); ch.send(10);`)
* **Metaprogramação (Potencial):** Macros ou `constexpr` para execução de código em tempo de compilação, permitindo otimizações e geração de código especializado.

## Impacto Potencial da GreenP

Acreditamos que a adoção generalizada da GreenP pode levar a uma redução substancial no consumo de energia do software global. Ao capacitar desenvolvedores com uma linguagem que é energeticamente eficiente por design, podemos:

* **Diminuir os custos operacionais** de data centers e infraestruturas de nuvem.
* **Aumentar a vida útil da bateria** de dispositivos móveis e IoT.
* **Reduzir a pegada de carbono** da indústria de tecnologia.
* **Permitir que software avançado rode em hardware com menos recursos**, estendendo a vida útil do hardware e promovendo a inclusão digital.
* **Contribuir ativamente para as metas da Agenda 2030 da ONU** relacionadas à energia sustentável e ação climática.

## Casos de Uso e Público-Alvo

GreenP é ideal para:

* **Desenvolvimento de Sistemas:** Sistemas operacionais, kernels, drivers.
* **Sistemas Embarcados e IoT:** Onde a eficiência energética e de recursos é primordial.
* **Computação de Alto Desempenho (HPC) Sustentável:** Aplicações científicas e de processamento de dados que exigem velocidade e baixo consumo.
* **Aplicações de Backend e Servidores:** Onde a otimização do consumo energético em escala pode gerar economias significativas.
* **Desenvolvimento de "IA Verde":** Ferramentas e modelos de inteligência artificial com menor impacto ambiental.

## Junte-se à Comunidade GreenP!

A GreenP é um projeto open source e acreditamos que a colaboração da comunidade é essencial para seu sucesso e para alcançarmos nossa visão de um futuro digital mais sustentável.

Convidamos você a explorar a GreenP, experimentar com a linguagem (à medida que os primeiros protótipos e o compilador se tornarem disponíveis) e contribuir para esta importante missão.
