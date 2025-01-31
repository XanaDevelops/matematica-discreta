import java.lang.AssertionError;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.IntPredicate;
import java.util.function.Predicate;

import javax.swing.text.html.HTMLDocument.RunElement;

/*
 * Aquesta entrega consisteix en implementar tots els mètodes annotats amb el comentari "// TO DO".
 *
 * L'avaluació consistirà en:
 *
 * - Si el codi no compila, la nota del grup serà de 0.
 *
 * - Principalment, el correcte funcionament de cada mètode (provant amb diferents entrades). Teniu
 *   alguns exemples al mètode `main`.
 *
 * - Tendrem en compte la neteja i organització del codi. Un estandard que podeu seguir és la guia
 *   d'estil de Google per Java: https://google.github.io/styleguide/javaguide.html.  Algunes
 *   consideracions importants: indentació i espaiat consistent, bona nomenclatura de variables,
 *   declarar les variables el més aprop possible al primer ús (és a dir, evitau blocs de
 *   declaracions). També convé utilitzar el for-each (for (int x : ...)) enlloc del clàssic (for
 *   (int i = 0; ...)) sempre que no necessiteu l'índex del recorregut.
 *
 * Per com està plantejada aquesta entrega, no necessitau (ni podeu) utilitzar cap `import`
 * addicional, ni mètodes de classes que no estiguin ja importades. El que sí podeu fer és definir
 * tots els mètodes addicionals que volgueu (de manera ordenada i dins el tema que pertoqui).
 *
 * Podeu fer aquesta entrega en grups de com a màxim 3 persones, i necessitareu com a minim Java 8.
 * Per entregar, posau a continuació els vostres noms i entregau únicament aquest fitxer.
 * - Nom 1: Marc Garcia Bonet
 * - Nom 2: Daniel García Vázquez
 * - Nom 3: Pere Garcias Miralles
 *
 * L'entrega es farà a través d'una tasca a l'Aula Digital que obrirem abans de la data que se us
 * hagui comunicat i vos recomanam que treballeu amb un fork d'aquest repositori per seguir més
 * fàcilment les actualitzacions amb enunciats nous. Si no podeu visualitzar bé algun enunciat,
 * assegurau-vos de que el vostre editor de texte estigui configurat amb codificació UTF-8.
 */
class Entrega {
  /*
   * Aquí teniu els exercicis del Tema 1 (Lògica).
   *
   * Els mètodes reben de paràmetre l'univers (representat com un array) i els
   * predicats adients
   * (per exemple, `Predicate<Integer> p`). Per avaluar aquest predicat, si `x` és
   * un element de
   * l'univers, podeu fer-ho com `p.test(x)`, que té com resultat un booleà (true
   * si `P(x)` és
   * cert). Els predicats de dues variables són de tipus `BiPredicate<Integer,
   * Integer>` i
   * similarment s'avaluen com `p.test(x, y)`.
   *
   * En cada un d'aquests exercicis us demanam que donat l'univers i els predicats
   * retorneu `true`
   * o `false` segons si la proposició donada és certa (suposau que l'univers és
   * suficientment
   * petit com per poder provar tots els casos que faci falta).
   */
  static class Tema1 {
    /*
     * És cert que ∀x ∃!y. P(x) -> Q(x,y) ?
     */
    static boolean exercici1(int[] universe, Predicate<Integer> p, BiPredicate<Integer, Integer> q) {
      if (universe.length == 0) { // univers buit, quant. universal cert
        return true;
      }

      for (int x : universe) {
        int count = 0;
        for (int y : universe) {
          if (!p.test(x) || q.test(x, y)) { // implicació
            count++;
          }
        }
        if (count != 1) { // existencia unica
          return false;
        }
      }
      return true;
    }

    /*
     * És cert que ∃!x ∀y. P(y) -> Q(x,y) ?
     */
    static boolean exercici2(int[] universe, Predicate<Integer> p, BiPredicate<Integer, Integer> q) {
      boolean buit = universe.length == 0; // Comprovam si l'univers és buit
      boolean solucio;
      if (!buit) { // Si l'univers no és buit, continuam
        boolean premisa = false;
        for (int y : universe) { // Comprovam si la premisa és certa en algun cas
          premisa = p.test(y);
          if (premisa) {
            break;
          }
        }

        if (premisa) { // Si la premisa és vertadera en algun cas, miram si es compleix la implicació
          int co = 0;
          for (int x : universe) {
            boolean trobat = false;
            for (int y : universe) {
              if (p.test(y) && !q.test(x, y)) { // Cercam casos on la implicació sigui falsa
                trobat = true;
                break;
              }
            }
            if (!trobat) { // Si per tot y la implicació és certa, augmentam el comptador
              co++;
            }
          }
          solucio = co == 1; // La proposició és certa només si s'ha complert per un sol x
        } else { // Si la premisa és sempre falsa, la proposició sempre és vertadera
          solucio = true;
        }
      } else {
        solucio = false; // Si l'univers és buit, el quantificador existencial sempre és fals
      }

      return solucio;
    }

    /*
     * És cert que ∃x,y ∀z. P(x,z) ⊕ Q(y,z) ?
     */
    static boolean exercici3(int[] universe, BiPredicate<Integer, Integer> p, BiPredicate<Integer, Integer> q) {

      if (universe.length == 0) {
        return false;
      }
      // Es miren totes les combinacions de x, y i z.
      for (int x : universe) {
        for (int y : universe) {

          // Booleana que indica que es produeix la xor per tot z
          boolean xor = true;

          for (int z : universe) { // Si no es produeix la xor en algún cas,
            if (!(p.test(x, z) ^ q.test(y, z))) {// passam a la següent iteració
              xor = false;
              break;
            }
          }
          if (xor) { // Si en acabar el bucle de la z xor = true,
            return true; // vol dir que totes les z ho compleixen
          }
        }
      } // Ja s'han mirat totes les x, y i z i xor no ha estat true
      if (universe.length == 0) {
        return false;
      }
      for (int x : universe) {
        for (int y : universe) {
          boolean xor = true;
          for (int z : universe) {
            if (!(p.test(x, z) ^ q.test(y, z))) {
              xor = false;
              break;
            }
          }
          if (xor) {
            return true;
          }
        }
      }
      return false;
    }

    /*
     * És cert que (∀x. P(x)) -> (∀x. Q(x)) ?
     */
    static boolean exercici4(int[] universe, Predicate<Integer> p, Predicate<Integer> q) {
      boolean contraexempleP = false;
      for (int x : universe) { // Comprovam si per tot x es compleix P(x)
        contraexempleP = !p.test(x);
        if (contraexempleP) { // Si per algun x no es compleix P(x), aleshores
          break; // la premisa és falsa i la implicació és sempre certa.
        }
      }

      boolean contraexempleQ = false; // Si l'univers és buit, aquest valor prevaldrà i la
                                      // proposició serà sempre certa per al quantificador universal.

      if (!contraexempleP) { // Comprovam si per tot x es compleix Q(x)
        for (int x : universe) {
          contraexempleQ = !q.test(x);
          if (contraexempleQ) { // Si per algun x no es compleix Q(x), aleshores la implicació és falsa
            break;
          }
        }
      }
      return (contraexempleP || !contraexempleQ);
    }

    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu
     * `main`)
     */
    static void tests() {
      // Exercici 1
      // ∀x ∃!y. P(x) -> Q(x,y) ?

      assertThat(
          exercici1(
              new int[] { 2, 3, 5, 6 },
              x -> x != 4,
              (x, y) -> x == y));

      assertThat(
          !exercici1(
              new int[] { -2, -1, 0, 1, 2, 3 },
              x -> x != 0,
              (x, y) -> x * y == 1));

      // Exercici 2
      // ∃!x ∀y. P(y) -> Q(x,y) ?

      assertThat(
          exercici2(
              new int[] { -1, 1, 2, 3, 4 },
              y -> y <= 0,
              (x, y) -> x == -y));

      assertThat(
          !exercici2(
              new int[] { -2, -1, 1, 2, 3, 4 },
              y -> y < 0,
              (x, y) -> x * y == 1));

      // Exercici 3
      // ∃x,y ∀z. P(x,z) ⊕ Q(y,z) ?

      assertThat(
          exercici3(
              new int[] { 2, 3, 4, 5, 6, 7, 8 },
              (x, z) -> z % x == 0,
              (y, z) -> z % y == 1));

      assertThat(
          !exercici3(
              new int[] { 2, 3 },
              (x, z) -> z % x == 1,
              (y, z) -> z % y == 1));

      // Exercici 4
      // (∀x. P(x)) -> (∀x. Q(x)) ?

      assertThat(
          exercici4(
              new int[] { 0, 1, 2, 3, 4, 5, 8, 9, 16 },
              x -> x % 2 == 0, // x és múltiple de 2
              x -> x % 4 == 0 // x és múltiple de 4
          ));

      assertThat(
          !exercici4(
              new int[] { 0, 2, 4, 6, 8, 16 },
              x -> x % 2 == 0, // x és múltiple de 2
              x -> x % 4 == 0 // x és múltiple de 4
          ));
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 2 (Conjunts).
   *
   * Per senzillesa tractarem els conjunts com arrays (sense elements repetits).
   * Per tant, un
   * conjunt de conjunts d'enters tendrà tipus int[][].
   *
   * Les relacions també les representarem com arrays de dues dimensions, on la
   * segona dimensió
   * només té dos elements. Per exemple
   * int[][] rel = {{0,0}, {1,1}, {0,1}, {2,2}};
   * i també donarem el conjunt on està definida, per exemple
   * int[] a = {0,1,2};
   *
   * Les funcions f : A -> B (on A i B son subconjunts dels enters) les
   * representam donant el domini
   * int[] a, el codomini int[] b, i f un objecte de tipus Function<Integer,
   * Integer> que podeu
   * avaluar com f.apply(x) (on x és d'a i el resultat f.apply(x) és de b).
   */
  static class Tema2 {
    /*
     * Comprovau si la relació `rel` definida sobre `a` és d'equivalència.
     *
     * Podeu soposar que `a` està ordenat de menor a major.
     */
    static boolean exercici1(int[] a, int[][] rel) {
      boolean reflexiva = true; // Primer, comprovam si és reflexiva
      for (int i : a) {
        boolean trobada = false;
        for (int[] j : rel) {
          if ((j[0] == i) && (j[1] == i)) {
            trobada = true;
            break;
          }
        }
        if (!trobada) { // Si no existeix l'element (i, i), no és reflexiva
          reflexiva = false;
          break;
        }
      }

      boolean simetrica = true; // Segon, comprovam si és simètrica
      for (int[] i : rel) {
        boolean trobada = false;
        for (int[] j : rel) {
          if ((j[0] == i[1]) && (j[1] == i[0])) {
            trobada = true;
            break;
          }
        }
        if (!trobada) { // Si per (i, j) no existeix (j, i), no és simètrica
          simetrica = false;
          break;
        }
      }

      boolean transitiva = true; // Tercer, comprovam si és transitiva
      for (int[] i : rel) {
        for (int[] j : rel) {
          if (j[0] == i[1]) {
            boolean trobada = false;
            for (int[] k : rel) {
              if ((k[0] == i[0]) && (k[1] == j[1])) {
                trobada = true;
                break;
              }
            }
            if (!trobada) { // Si existeixen (i, j) i (j, k), però no (i, k), no és transitiva
              transitiva = false;
              break;
            }
          }
        }
        if (!transitiva) {
          break;
        }
      }

      // És una relació d'equivalència si i només si és reflexiva, simètrica i
      // transitiva
      return (reflexiva && simetrica && transitiva);
    }

    /*
     * Comprovau si la relació `rel` definida sobre `a` és d'equivalència. Si ho és,
     * retornau el
     * cardinal del conjunt quocient de `a` sobre `rel`. Si no, retornau -1.
     *
     * Podeu soposar que `a` està ordenat de menor a major.
     */
    static int exercici2(int[] a, int[][] rel) {
      if (!(exercici1(a, rel))) {// Si no és d'equivalència retorna -1
        return -1;
      } else {

        List<Set<Integer>> classesEQ = new ArrayList<>();

        for (int[] fila : rel) {

          int x = fila[0]; // Primer valor de la fila
          int y = fila[1]; // Segon valor

          Set<Integer> classe1 = null; // Classe d'equivalència del primer valor
          Set<Integer> classe2 = null; // Classe d'equivalència del segon valor

          // For-each a través de les classes ja existents per veure si x o y ja hi son a
          // qualcuna
          for (Set<Integer> equivalenceClass : classesEQ) {
            if (equivalenceClass.contains(x)) { // S'ha trobat x a una classe
              classe1 = equivalenceClass;
            }
            if (equivalenceClass.contains(y)) { // S'ha trobat y a una classe
              classe2 = equivalenceClass;
            }
          }

          // Si ambdues classes són null és perque ni x ni y són a cap classe,doncs
          // es crea una classe i s'hi afegeixen tots dos.
          if (classe1 == null && classe2 == null) {
            Set<Integer> newClass = new HashSet<>();
            newClass.add(x);
            newClass.add(y);

            classesEQ.add(newClass);// S'afegeix la classe a la llista de classes
          } else if (classe1 == null) {// Si sa classe 1 és buida s'afegeix x a la classe de y
            classe2.add(x);
          } else if (classe2 == null) {// Si sa classe 2 és buida s'afegeix y a la classe de x
            classe1.add(y);
          } else if (classe1 != classe2) {// Si cap classe és null es combinen i s'elimina la 2
            classe1.addAll(classe2);
            classesEQ.remove(classe2);
          }
        }
        // Es retorna la quantitat de classes d'equivalència
        return classesEQ.size();
      }
    }

    /*
     * Comprovau si la relació `rel` definida entre `a` i `b` és una funció.
     *
     * Podeu soposar que `a` i `b` estan ordenats de menor a major.
     */
    static boolean exercici3(int[] a, int[] b, int[][] rel) {
      // ∀a ∈ A : ∃!b ∈ B : (a, b) ∈ Γf
      if (a.length == 0) {
        return true;
      }
      if (b.length == 0 || rel.length == 0) { // prevenir buit
        return false;
      }

      for (int x : a) { // per cada element de A
        int count = 0;
        for (int y : b) { // i de B
          for (int[] r : rel) { // si existeix (a,b)
            if (Arrays.equals(r, new int[] { x, y })) {
              count++;
            }
          }
        }
        if (count > 1) { // si a te més d'una imatge
          return false;
        }
      }
      return true;
    }

    /*
     * Suposau que `f` és una funció amb domini `dom` i codomini `codom`. Retornau:
     * - Si és exhaustiva, el màxim cardinal de l'antiimatge de cada element de
     * `codom`.
     * - Si no, si és injectiva, el cardinal de l'imatge de `f` menys el cardinal de
     * `codom`.
     * - En qualsevol altre cas, retornau 0.
     *
     * Podeu suposar que `dom` i `codom` estàn ordenats de menor a major.
     */
    static int exercici4(int[] dom, int[] codom, Function<Integer, Integer> f) {
      int[] comptImatges = new int[codom.length]; // Array on cada índex correspon al nombre d'elements del domini que
                                                  // van a
                                                  // parar a l'element del mateix índex del codomini quan se'ls aplica
                                                  // f.

      int resultat = 0;
      for (int i : dom) {
        for (int j : codom) {
          if (f.apply(i) == j) {
            comptImatges[j]++;
          }
        }
      }

      boolean exhaustiva = true; // Comprovam si és exhaustiva
      int max = 0;
      for (int i : comptImatges) { // Cercam el número d'antiimatges de l'element del codomini amb més antiimatges
        if (i > max) {
          max = i;
        }
        exhaustiva = i != 0; // Si algun comptador està a 0, algun element del codomini no prové del domini i
                             // no és exhaustiva
        if (!exhaustiva) {
          break;
        }
      }

      if (!exhaustiva) {
        boolean injectiva = true; // Si no és exhaustiva, comprovam si és injectiva
        int cardinalIm = 0;
        for (int i : comptImatges) { // Comptam els elements del domini que tenen imatge en el codomini
          if (i > 0) {
            cardinalIm++;
          }
          injectiva = !(i > 1); // Si algun element del codomini té més d'una antiimatge dins el domini, no és
                                // injectiva.

          if (!injectiva) {
            break;
          }
        }

        if (injectiva) {
          resultat = cardinalIm - codom.length;
        }

      } else {
        resultat = max;
      }

      return resultat;
    }

    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu
     * `main`)
     */
    static void tests() {
      // Exercici 1
      // `rel` és d'equivalencia?

      assertThat(
          exercici1(
              new int[] { 0, 1, 2, 3 },
              new int[][] { { 0, 0 }, { 1, 1 }, { 2, 2 }, { 3, 3 }, { 1, 3 }, { 3, 1 } }));

      assertThat(
          !exercici1(
              new int[] { 0, 1, 2, 3 },
              new int[][] { { 0, 0 }, { 1, 1 }, { 2, 2 }, { 3, 3 }, { 1, 2 }, { 1, 3 }, { 2, 1 }, { 3, 1 } }));

      // Exercici 2
      // si `rel` és d'equivalència, quants d'elements té el seu quocient?

      final int[] int09 = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };

      assertThat(
          exercici2(
              int09,
              generateRel(int09, int09, (x, y) -> x % 3 == y % 3)) == 3);

      assertThat(
          exercici2(
              new int[] { 1, 2, 3 },
              new int[][] { { 1, 1 }, { 2, 2 } }) == -1);

      // Exercici 3
      // `rel` és una funció?

      final int[] int05 = { 0, 1, 2, 3, 4, 5 };

      assertThat(
          exercici3(
              int05,
              int09,
              generateRel(int05, int09, (x, y) -> x == y)));

      assertThat(
          !exercici3(
              int05,
              int09,
              generateRel(int05, int09, (x, y) -> x == y / 2)));

      // Casos propios
      final int[] intM55 = { -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5 };
      final int[] int036 = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
          25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35 };

      assertThat(!exercici3(int05, int09, generateRel(int05, int09, (x, y) -> x > y)));
      assertThat(!exercici3(int05, int09, generateRel(int05, int09, (x, y) -> x <= y)));
      assertThat(!exercici3(int09, int05, generateRel(int09, int05, (x, y) -> x == 20)));
      assertThat(exercici3(int09, int09, generateRel(int09, int09, (x, y) -> x == 2 * y + 1)));
      assertThat(!exercici3(int036, intM55, generateRel(int036, intM55, (x, y) -> x == y * y)));
      // Exercici 4
      // el major |f^-1(y)| de cada y de `codom` si f és exhaustiva
      // sino, |im f| - |codom| si és injectiva
      // sino, 0

      assertThat(
          exercici4(
              int09,
              int05,
              x -> x / 4) == 0);

      assertThat(
          exercici4(
              int05,
              int09,
              x -> x + 3) == int05.length - int09.length);

      assertThat(
          exercici4(
              int05,
              int05,
              x -> (x + 3) % 6) == 1);
    }

    /// Genera un array int[][] amb els elements {a, b} (a de as, b de bs) que
    /// satisfàn pred.test(a, b)
    static int[][] generateRel(int[] as, int[] bs, BiPredicate<Integer, Integer> pred) {
      ArrayList<int[]> rel = new ArrayList<>();

      for (int a : as) {
        for (int b : bs) {
          if (pred.test(a, b)) {
            rel.add(new int[] { a, b });
          }
        }
      }

      return rel.toArray(new int[][] {});
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 3 (Grafs).
   *
   * Donarem els grafs en forma de diccionari d'adjacència, és a dir, un graf serà
   * un array
   * on cada element i-èssim serà un array ordenat que contendrà els índexos dels
   * vèrtexos adjacents
   * al i-èssim vèrtex. Per exemple, el graf cicle C_3 vendria donat per
   *
   * int[][] g = {{1,2}, {0,2}, {0,1}} (no dirigit: v0 -> {v1, v2}, v1 -> {v0,
   * v2}, v2 -> {v0,v1})
   * int[][] g = {{1}, {2}, {0}} (dirigit: v0 -> {v1}, v1 -> {v2}, v2 -> {v0})
   *
   * Podeu suposar que cap dels grafs té llaços.
   */
  static class Tema3 {

    /*
     * Retornau l'ordre menys la mida del graf (no dirigit).
     */
    static int exercici1(int[][] g) {
      int mida = 0;
      int ordre = 0;

      // Bucles for per recórrer tots els elements de la matriu

      for (int i = 0; i < g.length; i++) {
        for (int j = 0; j < g[i].length; j++) {

          if (i < g[i][j]) {// Com que els nodes estan ordenats, si a la fila i trobam un nombre major,
                            // es sap que hi ha una aresta que no s'ha comptat, però si es troba
                            // un nombre menor que i, sabem que aqueixa aresta ja s'ha sumat abans

            mida++;
          }
        }
        ordre++; // A cada fila de la matriu es suma 1 perque una fila representa un node
      }
      return ordre - mida;
    }

    static class Node {
      public int dest, peso; // b=desti, c=pes

      public Node(int dest, int peso) {
        this.dest = dest;
        this.peso = peso;
      }

      public boolean menorPesQue(Node b) {
        return peso < b.peso;
      }
    }

    static void colaAdd(ArrayList<Node> punts, Node node) {
      for (int i = 0; i < punts.size(); i++) {
        if (node.menorPesQue(punts.get(i))) {
          punts.add(i, node);
          return;
        }
      }
      punts.add(node);
    }

    static int[] dijkstra(ArrayList<Node>[] punts, int nodes, int origen) {
      // trobar distancia minima des de origen a tots els altres
      int[] dist = new int[nodes];
      Arrays.fill(dist, Integer.MAX_VALUE);

      boolean[] visit = new boolean[nodes];
      Arrays.fill(visit, false);
      // se que no es una cola, no la podem importar...
      ArrayList<Node> cola = new ArrayList<>();

      dist[origen] = 0;
      cola.add(new Node(origen, 0));
      while (!cola.isEmpty()) {
        Node p = cola.get(0);
        cola.remove(0);

        for (Node destPunt : punts[p.dest]) {
          if (visit[destPunt.dest]) {
            continue;
          }
          if (dist[destPunt.dest] > dist[p.dest] + destPunt.peso) {
            dist[destPunt.dest] = dist[p.dest] + destPunt.peso;
            colaAdd(cola, new Node(destPunt.dest, dist[destPunt.dest]));
          }
        }
      }
      return dist;
    }

    /*
     * Suposau que el graf (no dirigit) és connex. És bipartit?
     */
    static boolean exercici2(int[][] g) {
      // va "pintant" amb colors 1 i -1 el graf, si el mateix color es adjacent
      // no es bipartit
      int[] conex = new int[g.length]; // 0=desconocido, 1, -1 grupos
      Arrays.fill(conex, 0);

      ArrayList<Integer> cola = new ArrayList<Integer>();
      conex[0] = 1;

      cola.add(0);
      while (!cola.isEmpty()) {
        int ori = cola.get(0);
        int valorOri = conex[ori];
        cola.remove(0);
        for (int destino : g[ori]) {
          if (conex[destino] == 0) {
            conex[destino] = valorOri * -1;
            cola.add(destino);
            continue;
          }
          if (conex[destino] == valorOri) {
            return false;
          }
        }
      }
      return true;
    }

    /*
     * Suposau que el graf és un DAG. Retornau el nombre de descendents amb grau de
     * sortida 0 del
     * vèrtex i-èssim.
     */
    static int exercici3(int[][] g, int i) {
      int co = 0;
      int[] nodesVisitats = new int[g.length]; // Aquí es guarden els nodes que s'han comprovat
      int darrer = 0; // Índex que marca la posició del darrer element de nodesVisitats
      int[] sequencia = new int[g.length]; // Aquí s'hi guarden, per ordre, els nodes amb descendents que es van
                                           // recorrent
      int posPare = 0; // Índex que marca la posició del node pare sobre el que s'està situat
      boolean acabar = false;

      if (g[i].length == 0) { // Si el node inicial no té descendents, el programa s'acaba
        acabar = true;
      } else {
        nodesVisitats[darrer] = i;
        sequencia[posPare] = i;
      }

      // NO ES FA SERVIR FOR EACH PERQUÈ ES NECESSITA OPERAR AMB L'ÍNDEX j
      while (!acabar) {
        for (int j = 0; j < g[sequencia[posPare]].length; j++) {
          int nodeFill = g[sequencia[posPare]][j]; // Situats sobre un node pare, es miren els seus adjacents

          if (!esRepetit(nodesVisitats, darrer, nodeFill)) { // Si no s'ha visitat, es mira si té descendents
            if (g[nodeFill].length > 0) { // Si té descendents, ens situam sobre ell com a nou node pare
              nodesVisitats[++darrer] = nodeFill;
              sequencia[++posPare] = nodeFill;
              j--;
            } else { // Si no té descendents, incrementam el comptador
              nodesVisitats[++darrer] = nodeFill;
              co++;
            }
          }

          if (fillsVisitats(g, nodesVisitats, darrer, sequencia[posPare])) { // Quan s'han vistat tots els nodes
                                                                             // adjacents d'un pare,
            posPare--; // retrocedim al node pare anterior.

            if (posPare < 0) { // El programa acaba quan arribam al node original i se n'han comprovat tots els
                               // descendents
              acabar = true;
              posPare = 0;
            }

          }

        }

      }
      return co;
    }

    // Mètode propi: retorna si un node ja ha estat visitat
    private static boolean esRepetit(int[] visitats, int darrer, int node) {
      boolean repetit = false;
      for (int i = 0; !repetit && i <= darrer; i++) {
        repetit = visitats[i] == node;
      }
      return repetit;
    }

    // Mètode propi: si tots els nodes adjacents d'un node (pare) han estat visitats
    private static boolean fillsVisitats(int[][] g, int[] visitats, int darrer, int pare) {
      boolean totsVisitats = true;
      for (int i = 0; totsVisitats && i < g[pare].length; i++) {
        totsVisitats = esRepetit(visitats, darrer, g[pare][i]);
      }
      return totsVisitats;
    }

    /*
     * Donat un arbre arrelat (dirigit, suposau que l'arrel es el vèrtex 0),
     * trobau-ne el diàmetre
     * del graf subjacent. Suposau que totes les arestes tenen pes 1.
     */
    static int exercici4(int[][] g) {
      // Preparar per dijsktra
      ArrayList<Node>[] punts = new ArrayList[g.length];
      for (int i = 0; i < punts.length; i++) {
        punts[i] = new ArrayList<>();
      }
      for (int i = 0; i < g.length; i++) {
        for (int d : g[i]) {
          punts[i].add(new Node(d, 1));
          punts[d].add(new Node(i, 1));
        }
      }
      // cercar el punt més distant
      int[] dist = dijkstra(punts, g.length, 0);
      // System.out.println(Arrays.toString(dist));
      int max = -1;
      int imax = 0;
      for (int i = 0; i < dist.length; i++) {
        if (dist[i] > max) {
          max = dist[i];
          imax = i;
        }
      }
      // des del node més distant, trobar el més distant, en cas d'arbres, diametre
      int[] dist2 = dijkstra(punts, g.length, imax);
      // System.out.println(Arrays.toString(dist2));
      max = -1;
      imax = 0;
      for (int i = 0; i < dist2.length; i++) {
        if (dist2[i] > max) {
          max = dist2[i];
          imax = i;
        }
      }
      return max; // TO DO
    }

    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu
     * `main`)
     */
    static void tests() {
      final int[][] undirectedK6 = {
          { 1, 2, 3, 4, 5 },
          { 0, 2, 3, 4, 5 },
          { 0, 1, 3, 4, 5 },
          { 0, 1, 2, 4, 5 },
          { 0, 1, 2, 3, 5 },
          { 0, 1, 2, 3, 4 },
      };

      /*
       * 1
       * 4 0 2
       * 3
       */
      final int[][] undirectedW4 = {
          { 1, 2, 3, 4 },
          { 0, 2, 4 },
          { 0, 1, 3 },
          { 0, 2, 4 },
          { 0, 1, 3 },
      };

      // 0, 1, 2 | 3, 4
      final int[][] undirectedK23 = {
          { 3, 4 },
          { 3, 4 },
          { 3, 4 },
          { 0, 1, 2 },
          { 0, 1, 2 },
      };

      /*
       * 7
       * 0
       * 1 2
       * 3 8
       * 4
       * 5 6
       */
      final int[][] directedG1 = {
          { 1, 2 }, // 0
          { 3 }, // 1
          { 3, 8 }, // 2
          { 4 }, // 3
          { 5, 6 }, // 4
          {}, // 5
          {}, // 6
          { 0 }, // 7
          {},
      };

      /*
       * 0
       * 1 2 3
       * 4 5 6
       * 7 8
       */

      final int[][] directedRTree1 = {
          { 1, 2, 3 }, // 0 = r
          {}, // 1
          { 4, 5 }, // 2
          { 6 }, // 3
          { 7, 8 }, // 4
          {}, // 5
          {}, // 6
          {}, // 7
          {}, // 8
      };

      /*
       * 0
       * 1
       * 2 3
       * 4 5
       * 6 7
       */

      final int[][] directedRTree2 = {
          { 1 },
          { 2, 3 },
          {},
          { 4, 5 },
          {},
          { 6, 7 },
          {},
          {},
      };

      final int[][] undirectedG7 = {
          { 6 },
          { 5, 6 },
          { 6 },
          { 4, 6 },
          { 3, 5 },
          { 1, 4 },
          { 0, 1, 2, 3 }
      };

      final int[][] undirectedW3 = {
          { 1, 2 },
          { 0, 2 },
          { 0, 1 }
      };

      final int[][] undirectedP2 = {
          { 1 },
          { 0 }
      };

      final int[][] directedRTree3 = {
          { 1, 2 },
          { 3, 4 },
          { 5, 6 },
          {},
          {},
          {},
          {}
      };

      assertThat(exercici1(undirectedK6) == 6 - 5 * 6 / 2);
      assertThat(exercici1(undirectedW4) == 5 - 2 * 4);

      assertThat(exercici2(undirectedK23));
      assertThat(!exercici2(undirectedK6));
      // casos propios
      assertThat(!exercici2(undirectedG7));
      assertThat(!exercici2(undirectedW3));
      assertThat(exercici2(directedRTree1));
      assertThat(exercici2(undirectedP2));

      assertThat(exercici3(directedG1, 0) == 3);
      assertThat(exercici3(directedRTree1, 2) == 3);

      assertThat(exercici4(directedRTree1) == 5);
      assertThat(exercici4(directedRTree2) == 4);
      assertThat(exercici4(directedRTree3) == 4);
    }
  }

  /*
   * Aquí teniu els exercicis del Tema 4 (Aritmètica).
   *
   * Per calcular residus podeu utilitzar l'operador %, però anau alerta amb els
   * signes.
   * Podeu suposar que cada vegada que se menciona un mòdul, és major que 1.
   */
  static class Tema4 {
    static int mod(int a, int b) {
      int res = a % b;
      if (res >= 0) {
        return res;
      }
      return res + Math.abs(b);
    }

    static int mcd(int a, int b) {
      return euclides(a, b)[0];
    }

    static int mcm(int a, int b) {
      return (a * b) / mcd(a, b);
    }

    static int[] euclides(int a, int b) {
      // System.out.println("Ec " + a + " " + b);
      int r1 = a, r2 = b, q, x1 = 1, x2 = 0, y1 = 0, y2 = 1;
      // if (a < b) {
      // System.err.println("A es MENOR que B");
      // }

      while (r2 != 0) {
        q = r1 / r2;
        // System.out.println("-> "+List.of(r1, r2, q, x1, x2, y1, y2));
        int res = r1 % r2;
        r1 = r2;
        r2 = res;
        int x3 = x1 - x2 * q;
        x1 = x2;
        x2 = x3;
        int y3 = y1 - y2 * q;
        y1 = y2;
        y2 = y3;
      }
      // mcd(a,b)=a*x+b*y
      // devuelve {mcd(a,b), x,y}
      if (r1 != x1 * a + y1 * b) {
        System.err.println("ERROR EUCLIDES");
      }
      return new int[] { r1, x1, y1 };
    }

    /*
     * Donau la solució de l'equació
     *
     * ax ≡ b (mod n),
     *
     * Els paràmetres `a` i `b` poden ser negatius (`b` pot ser zero), però podeu
     * suposar que n > 1.
     *
     * Si la solució és x ≡ c (mod m), retornau `new int[] { c, m }`, amb 0 ⩽ c < m.
     * Si no en té, retornau null.
     */

    static int[] exercici1(int a, int b, int n) {
      int[] e;
      e = euclides(a, n);

      // System.out.println(Arrays.toString(e));
      int d = e[0];
      if (b % d != 0) {
        // System.out.println("no divide");
        return null;
      }
      for (int i = 0; i < e.length; i++) {
        e[i] *= (b / d);
      }
      // System.out.println(Arrays.toString(e));
      int[] r = new int[] { mod(e[1], n / d), Math.abs(n / d) };
      // System.out.println("r " + Arrays.toString(r));
      return r;
    }

    /*
     * Donau la solució (totes) del sistema d'equacions
     *
     * { x ≡ b[0] (mod n[0])
     * { x ≡ b[1] (mod n[1])
     * { x ≡ b[2] (mod n[2])
     * { ...
     *
     * Cada b[i] pot ser negatiu o zero, però podeu suposar que n[i] > 1. També
     * podeu suposar
     * que els dos arrays tenen la mateixa longitud.
     *
     * Si la solució és de la forma x ≡ c (mod m), retornau `new int[] { c, m }`,
     * amb 0 ⩽ c < m.
     * Si no en té, retornau null.
     */
    static int[] exercici2a(int[] b, int[] n) {
      // EN AQUEST EXERCICI NO ES FA SERVIR FOR EACH PERQUÈ O BÉ NO ES RECORR TOT
      // L'ARRAY O BÉ ES NECESSITA L'ÍNDEX
      boolean coprimers = true;
      for (int i = 0; coprimers && i < n.length - 1; i++) { // Comprovam si té solució mirant si són coprimers dos a
                                                            // dos.
        coprimers = mcd(n[i + 1], n[i]) == 1;
      }

      boolean teSolucio = true;
      int[] solucio = new int[2]; // Si no són coprimers dos a dos, miram si té solució amb el mcd
      if (!coprimers) {
        for (int i = 0; teSolucio && i < n.length - 1; i++) {
          teSolucio = mod(b[i + 1] - b[i], mcd(n[i + 1], n[i])) == 0;
        }
      }
      if (teSolucio) {
        int[] p = new int[n.length]; // Cercam cada Pi
        for (int i = 0; i < p.length; i++) {
          p[i] = 1;
          for (int j = 0; j < n.length; j++) {
            if (j != i) {
              p[i] *= n[j];
            }
          }
        }

        int[] q = new int[n.length]; // Cercam cada Qi
        for (int i = 0; i < q.length; i++) {
          int[] aux = new int[3];
          int residu = mod(p[i], n[i]);
          aux = euclides(n[i], residu);
          q[i] = aux[2];
        }

        int sumatori = 0; // La suma de tots els grups Pi·Qi·bi
        int productori = 1; // El producte de totes les n
        for (int i = 0; i < n.length; i++) {
          sumatori += p[i] * q[i] * b[i];
          productori *= n[i];
        }

        while (sumatori < 0) { // Augmentam el sumatori fins que sigui positiu
          sumatori += productori;
        }

        while (sumatori >= productori) { // Decrementam el sumatori fins que sigui menor que el productori
          sumatori -= productori;
        }

        solucio[0] = sumatori;
        solucio[1] = productori;

      } else {
        solucio = null;
      }
      return solucio;
    }

    /*
     * Donau la solució (totes) del sistema d'equacions
     *
     * { a[0]·x ≡ b[0] (mod n[0])
     * { a[1]·x ≡ b[1] (mod n[1])
     * { a[2]·x ≡ b[2] (mod n[2])
     * { ...
     *
     * Cada a[i] o b[i] pot ser negatiu (b[i] pot ser zero), però podeu suposar que
     * n[i] > 1. També
     * podeu suposar que els tres arrays tenen la mateixa longitud.
     *
     * Si la solució és de la forma x ≡ c (mod m), retornau `new int[] { c, m }`,
     * amb 0 ⩽ c < m.
     * Si no en té, retornau null.
     */
    static int[] exercici2b(int[] a, int[] b, int[] n) {
      int c = 0;
      int m = 1;// Inicialització del mòdul

      for (int i = 0; i < a.length; i++) {

        // Sumam el mod perque 'a' sigui positiu i l'eqüació no canvii
        while (a[i] < 0) {
          a[i] += n[i];
          b[i] += n[i];
        }

        // Reduim a i b als seus residus modul n
        a[i] %= n[i];
        b[i] %= n[i];

        // Si un dels dos elements és 0 no seran coprimers
        if (a[i] == 0 || n[i] == 0) {
          return null;
        }

        int mcd = mcd(a[i], n[i]);
        // Si b[i] no es divisible pel mcd de a[i] i n[i] no té solució
        if (b[i] % mcd != 0) {
          return null;
        }

        // Es divideix tot entre el mcd per simplificar les equacions
        a[i] /= mcd;
        b[i] /= mcd;
        n[i] /= mcd;

        // Si n és 0 no té solució
        if (n[i] == 0) {
          return null;
        }

        // Invers modular de a modul n
        int q = (b[i] * inversModular(a[i], n[i])) % n[i];
        if ((q - c) % mcd(m, n[i]) != 0) {
          return null;
        }

        // Calculam el teorema xinès del residu
        c = TCR(c, m, q, n[i]);

        // Actualització del mòdul
        m *= n[i];
      }

      // Si la solució és negativa li sumam el mòdul perque sigui positiva i no canvii
      if (c < 0) {
        c += m;
      }
      return new int[] { c, m };
    }

    static int inversModular(int a, int m) {
      int m1 = m;
      int y = 0, x = 1;

      // Si el mòdul és 1 l'invers no està definit
      if (m1 == 1) {
        return 0;
      }

      // Es fa l'algorisme estès d'Euclides
      while (a > 1 && m != 0) {
        int q = a / m;
        int r = m;

        m = a % m;
        a = r;
        r = y;

        y = x - q * y;
        x = r;
      }

      // Si l'invers és negatiu es suma el mòdul fins que sigui positiu
      if (x < 0) {
        x += m1;
      }

      return x;
    }

    static int TCR(int x1, int m1, int x2, int m2) {
      // Si algún mòdul és 0 l'ecuació no té solució
      if (m1 == 0 || m2 == 0) {
        return 0;
      }
      // Calculam l'invers modular de m1 respecte a m2
      int inversM = inversModular(m1, m2);
      // A partir d'això calculam el teorema xinès del residu
      return ((x2 - x1) * inversM % m2 * m1 + x1) % (m1 * m2);
    }

    /*
     * Suposau que n > 1. Donau-ne la seva descomposició en nombres primers,
     * ordenada de menor a
     * major, on cada primer apareix tantes vegades com el seu ordre. Per exemple,
     *
     * exercici4a(300) --> new int[] { 2, 2, 3, 5, 5 }
     *
     * No fa falta que cerqueu algorismes avançats de factorització, podeu utilitzar
     * la força bruta
     * (el que coneixeu com el mètode manual d'anar provant).
     */
    static ArrayList<Integer> exercici3a(int n) {
      ArrayList<Integer> factors = new ArrayList<>();
      if (n < 0) {
        n *= -1;
      }
      int div = 2;
      if (n == 2 || n == 3) {
        factors.add(n);
        return factors;
      }
      final int startN = n;
      while (n >= div && n != 1) {
        if (n % div == 0) {
          factors.add(div);
          n /= div;
        } else {
          if (div >= 3) { // a partir del 3 anar sumant en dos en dos
            div += 2;
          } else {
            div++;
          }
        }
      }
      if (factors.isEmpty()) {
        // es primo
        factors.add(startN);
      }
      return factors;
    }

    /*
     * Retornau el nombre d'elements invertibles a Z mòdul n³.
     *
     * Alerta: podeu suposar que el resultat hi cap a un int (32 bits a Java), però
     * n³ no té perquè.
     * De fet, no doneu per suposat que pogueu tractar res més gran que el resultat.
     *
     * No podeu utilitzar `long` per solucionar aquest problema. Necessitareu
     * l'exercici 3a.
     */
    static int exercici3b(int n) {

      int phi = 1;// phi=1 per anar multiplicant-lo

      ArrayList<Integer> factorsN = new ArrayList();// ArrayList on es guardara la factorització de n
      factorsN = exercici3a(n);

      ArrayList<Integer> factorsN3 = triplicarArrayList(factorsN);// Cada element de factorsN es
                                                                  // guarda tres vegades i ordenat per
                                                                  // representar la factorització de n^3
      int[] nombres = new int[factorsN3.size()];
      int index = 0; // Index per nombres[]

      for (int i = 0; i < factorsN3.size(); i++) {

        int valor = factorsN3.get(i);// Es guarda el valor de la posició i de factorsN3
        int exponent = numeroApariciones(factorsN3, valor);

        nombres[index] = (int) Math.pow(valor, exponent); // Guarda valor elevat a tantes vegades
                                                          // com apareix aquell nombre dins
                                                          // l'ArrayList a nombres[]
        index++;
        i += exponent - 1; // Ens movem exponent-1 posicions per no repetir nombres
      }

      for (int i = 0; i < nombres.length; i++) {
        if (nombres[i] > 0) { // Major que 0 per si l'array no s'ha omplit

          phi *= phi(nombres[i]);// A phi se li multipliquen els phis de
                                 // tots els nombres de la factorització
        }
      }
      return phi;
    }

    public static int phi(int n) {
      int phi = n;// Inicialitza phi a n

      // Recorr els possibles factors primers de n
      for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) { // Si n mod i és 0
          while (n % i == 0) { // Es van eliminant tots els múltiples de i a 'n'
            n /= i;
          }
          phi -= phi / i; // Es resten tots els nombres que s'han llevat
        }
      }
      if (n > 1) {
        phi = phi / n; // En cas de que n sigui un nombre primer falta un factor primer més
      }
      return phi;
    }

    public static int numeroApariciones(ArrayList<Integer> arrayList, int nombre) {
      // Es conten les vegades que apareix el mateix nombre dins l'ArrayList
      int count = 0;
      for (int num : arrayList) {
        if (num == nombre) {// Si el nombre dins l'array és igual al paràmetre contador++
          count++;
        }
      }
      return count;
    }

    public static ArrayList<Integer> triplicarArrayList(ArrayList<Integer> list) {
      ArrayList<Integer> novaLlista = new ArrayList<>();

      // Es crea un nou ArrayList i s'hi afegeix cada element del paràmetre tres pics.
      for (int num : list) {
        novaLlista.add(num);
        novaLlista.add(num);
        novaLlista.add(num);
      }

      return novaLlista;
    }

    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu
     * `main`)
     */
    static void tests() {
      assertThat(Arrays.equals(exercici1(17, 1, 30), new int[] { 23, 30 }));
      assertThat(Arrays.equals(exercici1(-2, -4, 6), new int[] { 2, 3 }));
      assertThat(exercici1(2, 3, 6) == null);
      // test propios
      assertThat(Arrays.equals(exercici1(-42, 0, 35), new int[] { 0, 5 }));
      assertThat(Arrays.equals(exercici1(6, 26, 22), new int[] { 8, 11 }));
      assertThat(Arrays.equals(exercici1(-6, 26, 22), new int[] { 3, 11 }));
      assertThat(Arrays.equals(exercici1(6, -26, 22), new int[] { 3, 11 }));
      assertThat(Arrays.equals(exercici1(-6, -26, 22), new int[] { 8, 11 }));

      assertThat(
          exercici2a(
              new int[] { 1, 0 },
              new int[] { 2, 4 }) == null);

      assertThat(
          Arrays.equals(
              exercici2a(
                  new int[] { 3, -1, 2 },
                  new int[] { 5, 8, 9 }),
              new int[] { 263, 360 }));

      assertThat(
          exercici2b(
              new int[] { 1, 1 },
              new int[] { 1, 0 },
              new int[] { 2, 4 }) == null);

      assertThat(
          Arrays.equals(
              exercici2b(
                  new int[] { 2, -1, 5 },
                  new int[] { 6, 1, 1 },
                  new int[] { 10, 8, 9 }),
              new int[] { 263, 360 }));

      assertThat(exercici3a(10).equals(List.of(2, 5)));
      assertThat(exercici3a(1291).equals(List.of(1291)));
      assertThat(exercici3a(1292).equals(List.of(2, 2, 17, 19)));

      assertThat(exercici3a(13).equals(List.of(13)));
      assertThat(exercici3a(24).equals(List.of(2, 2, 2, 3)));
      assertThat(exercici3a(84).equals(List.of(2, 2, 3, 7)));
      assertThat(exercici3a(-30).equals(List.of(2, 3, 5)));
      assertThat(exercici3a(987654321).equals(List.of(3, 3, 17, 17, 379721)));

      assertThat(exercici3b(10) == 400);

      // Aquí 1292³ ocupa més de 32 bits amb el signe, però es pot resoldre sense
      // calcular n³.
      assertThat(exercici3b(1292) == 961_496_064);

      // Aquest exemple té el resultat fora de rang
      // assertThat(exercici3b(1291) == 2_150_018_490L);

    }

  }

  /*
   * Aquest mètode `main` conté alguns exemples de paràmetres i dels resultats que
   * haurien de donar
   * els exercicis. Podeu utilitzar-los de guia i també en podeu afegir d'altres
   * (no els tendrem en
   * compte, però és molt recomanable).
   *
   * Podeu aprofitar el mètode `assertThat` per comprovar fàcilment que un valor
   * sigui `true`.
   */
  public static void main(String[] args) {
    Tema1.tests();
    Tema2.tests();
    Tema3.tests();
    Tema4.tests();
  }

  /// Si b és cert, no fa res. Si b és fals, llança una excepció (AssertionError).
  static void assertThat(boolean b) {
    if (!b)
      throw new AssertionError();
  }
}

// vim: set textwidth=100 shiftwidth=2 expandtab :
