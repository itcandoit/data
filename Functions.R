#SBD 10/07/2025
# Fonction de clacul de  Within Inertie pour CHA
# ChatGPT utilisé pour certaines syntaxes

within_inertie <- function(data, clusters) {
  inertie_within <- 0
  
  # Pour chaque cluster
  for (k in unique(clusters)) {
    # Sélectionner les points du cluster k
    cluster_k <- data[clusters == k, , drop = FALSE]
    
    # Calculer le centre du cluster k (la moyenne des points)
    centre_k <- colMeans(cluster_k)
    
    # Calculer la somme des carrés des distances entre les points et le centre du cluster
    inertie_k <- sum(apply(cluster_k, 1, function(x) sum((x - centre_k)^2)))
    
    # Ajouter cette inertie à l'inertie totale
    inertie_within <- inertie_within + inertie_k
  }
  
  return(inertie_within)
}

# Fonction de clacul de  Between Inertie pour CHA
# ChatGPT utilisé pour certaines syntaxes

between_inertie <- function(data, clusters) {
  # Centre global (moyenne des données)
  centre_global <- colMeans(data)
  
  inertie_between <- 0
  
  # Pour chaque cluster
  for (k in unique(clusters)) {
    # Sélectionner les points du cluster k
    cluster_k <- data[clusters == k, , drop = FALSE]
    
    # Calculer le centre du cluster k
    centre_k <- colMeans(cluster_k)
    
    # Taille du cluster k
    nk <- nrow(cluster_k)
    
    # Ajouter la contribution du cluster k à l'inertie inter-classe
    inertie_between <- inertie_between + nk * sum((centre_k - centre_global)^2)
  }
  
  return(inertie_between)
}

#Ratio
ratio <- function (data,cluster) {
  b <- between_inertie(data, cluster)
  w <-  within_inertie(data, cluster)
  tot <- b + w
  r <- b/tot * 100 
  return (r)
}


