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
          if (!p.test(x) || q.test(x, y)) {
            count++;
          }
        }
        if (count != 1) {
          return false;
        }
      }
      return true;
    }

    /*
     * És cert que ∃!x ∀y. P(y) -> Q(x,y) ?
     */
    static boolean exercici2(int[] universe, Predicate<Integer> p, BiPredicate<Integer, Integer> q) {
      int co = 0;
      for (int i = 0; i < universe.length; i++) {
        boolean trobat = false;
        for (int j = 0; !trobat && j < universe.length; j++) {
          if (p.test(universe[j]) && !q.test(universe[i], universe[j])) {
            trobat = true;
          }
        }
        if (!trobat) {
          co++;
        }
      }
      return co == 1;
    }

    /*
     * És cert que ∃x,y ∀z. P(x,z) ⊕ Q(y,z) ?
     */
    static boolean exercici3(int[] universe, BiPredicate<Integer, Integer> p, BiPredicate<Integer, Integer> q) {

      if(universe.length==0){
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
      for (int i = 0; !contraexempleP && i < universe.length; i++) {
        contraexempleP = !p.test(universe[i]);
      }
      boolean contraexempleQ = false;
      if (!contraexempleP) {
        for (int i = 0; !contraexempleQ && i < universe.length; i++) {
          contraexempleQ = q.test(universe[i]);
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
      boolean reflexiva = true;
      for (int i = 0; reflexiva && i < a.length; i++) {
        boolean trobada = false;
        for (int j = 0; !trobada && j < rel.length; j++) {
          if ((rel[j][0] == a[i]) && (rel[j][1] == a[i])) {
            trobada = true;
          }
        }
        if (!trobada) {
          reflexiva = false;
        }
      }

      boolean simetrica = true;
      for (int i = 0; simetrica && i < rel.length; i++) {
        boolean trobada = false;
        for (int j = 0; !trobada && j < rel.length; j++) {
          if ((rel[j][0] == rel[i][1]) && (rel[j][1] == rel[i][0])) {
            trobada = true;
          }
        }
        if (!trobada) {
          simetrica = false;
        }
      }

      boolean transitiva = true;
      for (int i = 0; transitiva && i < rel.length; i++) {
        for (int j = 0; j < rel.length; j++) {
          if (rel[j][0] == rel[i][1]) {
            boolean trobada = false;
            for (int k = 0; !trobada && k < rel.length; k++) {
              if ((rel[k][0] == rel[i][0]) && (rel[k][1] == rel[j][1])) {
                trobada = true;
              }
            }
            if (!trobada) {
              transitiva = false;
            }
          }
        }
      }
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
      return 0; // TO DO
    }

    /*
     * Comprovau si la relació `rel` definida entre `a` i `b` és una funció.
     *
     * Podeu soposar que `a` i `b` estan ordenats de menor a major.
     */
    static boolean exercici3(int[] a, int[] b, int[][] rel) {
      if (a.length == 0 || b.length == 0 || rel.length == 0) { // prevenir buit
        return false;
      }
      for (int x : a) {
        int count = 0;
        for (int y : b) {
          for (int[] r : rel) {
            if (Arrays.equals(r, new int[] { x, y })) {
              count++;
            }
          }
        }
        if (count > 1) {
          return false;
        }
      }
      return true; // TO DO
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
      int[] comptImatges = new int[codom.length];
      int resultat = 0;
      for (int i = 0; i < dom.length; i++) {
        for (int j = 0; j < codom.length; j++) {
          if (f.apply(dom[i]) == codom[j]) {
            comptImatges[j]++;
          }
        }
      }

      boolean exhaustiva = true;
      int max = 0;
      for (int i = 0; exhaustiva && i < comptImatges.length; i++) {
        if (comptImatges[i] > max) {
          max = comptImatges[i];
        }
        exhaustiva = comptImatges[i] != 0;
      }

      if (!exhaustiva) {
        boolean injectiva = true;
        int cardinalIm = 0;
        for (int i = 0; injectiva && i < comptImatges.length; i++) {
          if (comptImatges[i] > 0) {
            cardinalIm++;
          }
          injectiva = !(comptImatges[i] > 1); 
        }

        if(injectiva) {
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
    static class Punt {
      public int dest, peso; // b=desti, c=pes

      public Punt(int dest, int peso) {
        this.dest = dest;
        this.peso = peso;
      }
    }

    static int[] dijkstra(ArrayList<Punt>[] punts, int nodes, int origen) {
      int[] dist = new int[nodes];
      Arrays.fill(dist, Integer.MAX_VALUE);
      boolean[] visit = new boolean[nodes];
      Arrays.fill(visit, false);
      ArrayList<Punt> cola = new ArrayList<>();
      dist[origen] = 0;
      cola.add(new Punt(origen, 0));
      while (!cola.isEmpty()) {
        Punt p = cola.get(0);
        cola.remove(0);
        for (Punt destPunt : punts[p.dest]) {
          if (visit[destPunt.dest]) {
            continue;
          }
          if (dist[destPunt.dest] > dist[p.dest] + destPunt.peso) {
            dist[destPunt.dest] = dist[p.dest] + destPunt.peso;
            cola.add(new Punt(destPunt.dest, dist[destPunt.dest]));
          }
        }
      }
      return dist;
    }

    static int exercici1(int[][] g) {
      return -1; // TO DO
    }

    /*
     * Suposau que el graf (no dirigit) és connex. És bipartit?
     */
    static boolean exercici2(int[][] g) {
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
      return true; // TO DO
    }

    /*
     * Suposau que el graf és un DAG. Retornau el nombre de descendents amb grau de
     * sortida 0 del
     * vèrtex i-èssim.
     */
    static int exercici3(int[][] g, int i) {
      int co = 0;
      int[] nodesVisitats = new int[g.length];
      int darrer = 0;
      int[] sequencia = new int[g.length];
      int posPare = 0;
      boolean acabar = false;

      if (g[i].length == 0) {
        acabar = true;
      } else {
        nodesVisitats[darrer] = i;
        sequencia[posPare] = i;
      }

      while (!acabar) {
        for (int j = 0; j < g[sequencia[posPare]].length; j++) {
          int nodeFill = g[sequencia[posPare]][j];

          if (!esRepetit(nodesVisitats, darrer, nodeFill)) {
            if (g[nodeFill].length > 0) {
              nodesVisitats[++darrer] = nodeFill;
              sequencia[++posPare] = nodeFill;
              j--;
            } else {
              nodesVisitats[++darrer] = nodeFill;
              co++;
            }
          }

          if (fillsVisitats(g, nodesVisitats, darrer, sequencia[posPare])) {
            posPare--;

            if (posPare < 0) {
              acabar = true;
              posPare = 0;
            }

          }

        }

      }
      return co;
    }

    // Mètode propi: retorna si un noda ja ha estat visitat
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
      ArrayList<Punt>[] punts = new ArrayList[g.length];
      for (int i = 0; i < punts.length; i++) {
        punts[i] = new ArrayList<>();
      }
      for (int i = 0; i < g.length; i++) {
        for (int d : g[i]) {
          punts[i].add(new Punt(d, 1));
          punts[d].add(new Punt(i, 1));
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

      assertThat(exercici1(undirectedK6) == 6 - 5 * 6 / 2);
      assertThat(exercici1(undirectedW4) == 5 - 2 * 4);

      assertThat(exercici2(undirectedK23));
      assertThat(!exercici2(undirectedK6));

      assertThat(exercici3(directedG1, 0) == 3);
      assertThat(exercici3(directedRTree1, 2) == 3);

      assertThat(exercici4(directedRTree1) == 5);
      assertThat(exercici4(directedRTree2) == 4);
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
      return -1; // TO DO
    }

    static int[] euclides(int a, int b) {
      System.out.println("Ec " + a + " " + b);
      int r1 = a, r2 = b, q, x1 = 1, x2 = 0, y1 = 0, y2 = 1;
      if (a < b) {
        System.err.println("A es MENOR que B");
      }

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

      return null; // TO DO
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
      return null; // TO DO
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
      return null; // TO DO
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
          if (div >= 3) {
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
      return -1; // TO DO
    }

    /*
     * Aquí teniu alguns exemples i proves relacionades amb aquests exercicis (vegeu
     * `main`)
     */
    static void tests() {
      assertThat(Arrays.equals(exercici1(-42, 0, 35), new int[] { 0, 5 })); // QUITAR

      assertThat(Arrays.equals(exercici1(17, 1, 30), new int[] { 23, 30 }));
      assertThat(Arrays.equals(exercici1(-2, -4, 6), new int[] { 2, 3 }));
      assertThat(exercici1(2, 3, 6) == null);
    }

    /*
     * assertThat(
     * exercici2a(
     * new int[] { 1, 0 },
     * new int[] { 2, 4 }) == null);
     * 
     * assertThat(
     * Arrays.equals(
     * exercici2a(
     * new int[] { 3, -1, 2 },
     * new int[] { 5, 8, 9 }),
     * new int[] { 263, 360 }));
     * 
     * assertThat(
     * exercici2b(
     * new int[] { 1, 1 },
     * new int[] { 1, 0 },
     * new int[] { 2, 4 }) == null);
     * 
     * assertThat(
     * Arrays.equals(
     * exercici2b(
     * new int[] { 2, -1, 5 },
     * new int[] { 6, 1, 1 },
     * new int[] { 10, 8, 9 }
     * ),
     * new int[] { 263, 360 }
     * )
     * );
     * 
     * assertThat(exercici3a(10).equals(List.of(2, 5)));
     * assertThat(exercici3a(1291).equals(List.of(1291)));
     * assertThat(exercici3a(1292).equals(List.of(2, 2, 17, 19 )));
     * 
     * assertThat(exercici3b(10) == 400);
     * 
     * // Aquí 1292³ ocupa més de 32 bits amb el signe, però es pot resoldre sense
     * // calcular n³.
     * assertThat(exercici3b(1292) == 961_496_064);
     * 
     * // Aquest exemple té el resultat fora de rang
     * // assertThat(exercici3b(1291) == 2_150_018_490);
     */
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
    try {
      Tema1.tests();
    } catch (AssertionError ex) {
      System.err.println("Error tema 1 " + ex.getStackTrace()[1]);
    }

    try {
      Tema2.tests();
    } catch (AssertionError ex) {
      System.err.println("Error tema 2 " + ex.getStackTrace()[1]);
    }
    try {
      Tema3.tests();
    } catch (AssertionError ex) {
      System.err.println("Error tema 3 " + ex.getStackTrace()[1]);
    }
    try {
      Tema4.tests();
    } catch (AssertionError ex) {
      System.err.println("Error tema 4 " + ex.getStackTrace()[1]);
    }
  }

  /// Si b és cert, no fa res. Si b és fals, llança una excepció (AssertionError).
  static void assertThat(boolean b) {
    if (!b)
      throw new AssertionError();
  }
}

// vim: set textwidth=100 shiftwidth=2 expandtab :
