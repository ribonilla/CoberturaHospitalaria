# CoberturaHospitalaria
Para las principales ciudades de Colombia se crea un mapa de calor de la cobertura geográfica de Hopitales y Clínicas a partir de la distancia en tiempo de acceso.

1. A partir de las direcciones físicas de las unidades de atención (en principio Hospitales y Clínicas) se determina su posición geográfica:Longitud y Latitud (herer::geocode())
2. Se generan puntos aleatorios sobre el mapa acotado por la dimensión goegráfica de la ciudad (sp::bbox)
3. Se calculan las distancias entre los puntos aleatorios y las unidades de atención (osrm::osrmTable)
4. Para cada punto aleatório se determina la mínima distancia.
5. Se interpolan las distancias de cada punto aleatorio generando el mapa de calor



