# christmas rayrender

# https://gist.github.com/tylermorganwall/b61cfff990925adfdddf3a4f1e47e3d3

# remotes::install_github('coolbutuseless/ggsvg')

library(rayrender)

#Generate CSG branches
branches0 = csg_cone(end=c(0,0.5,0)) %>% 
  csg_combine(csg_cone(start=c(0,0.25,0),end=c(0,0.75,0), radius=0.75), operation="blend", radius = 0.7)%>% 
  csg_scale(1.25)

branches1 = csg_cone(end=c(0,0.5,0)) %>% 
  csg_combine(csg_cone(start=c(0,0.25,0),end=c(0,0.75,0), radius=0.75), operation="blend", radius = 0.7)

branches2 = csg_cone(end=c(0,0.5,0)) %>% 
  csg_combine(csg_cone(start=c(0,0.25,0),end=c(0,0.75,0), radius=0.75), operation="blend", radius = 0.7) %>% 
  csg_scale(0.75)

branches3 = csg_cone(end=c(0,0.5,0)) %>% 
  csg_combine(csg_cone(start=c(0,0.25,0),end=c(0,0.75,0), radius=0.75), operation="blend", radius = 0.7) %>% 
  csg_scale(0.5)

#Generate evenly spaced ornaments and lights on tree by generating points on a cone
set.seed(1)
h = spacefillr::generate_pmj02bn_set(60)
bulbs = matrix(c(sqrt(h[,1])*cos(h[,2]*2*pi),sqrt(h[,1]),sqrt(h[,1])*sin(h[,2]*2*pi)),ncol=3)

christmas_light_colors = rep(c("#8934B8","#0A53DE", "#24D024", "#FBF21A", "#FB6F24", "#EA0D0D"),40)
h2 =  spacefillr::generate_pmj02bn_set(240,seed=1)
lights = matrix(c(sqrt(h2[,1])*cos(h2[,2]*2*pi),sqrt(h2[,1]),sqrt(h2[,1])*sin(h2[,2]*2*pi)),ncol=3)

redblue = sample(c("red","dodgerblue"), size=60,replace = T)

bulb_scene = list()
for(i in 1:nrow(bulbs)) {
  if(-2*bulbs[i,2]+1.6 < 1.4) {
    bulb_scene[[i]] = sphere(x=1.1*bulbs[i,1],y=-2*bulbs[i,2]+1.6,z=1.1*bulbs[i,3],
                             material=microfacet(color=redblue[i]),radius=0.1)
  }
}
bulbs_full = do.call(rbind,bulb_scene)


light_scene = list()
for(i in 1:nrow(lights)) {
  if(-2*lights[i,2]+1.6 < 1.4) {
    light_scene[[i]] = sphere(x=1.1*lights[i,1],y=-2.1*lights[i,2]+1.7,z=1.1*lights[i,3],
                              material=light(color=christmas_light_colors[i],importance_sample = F, intensity = 3),radius=0.02)
  }
}
lights_full = do.call(rbind,light_scene)

#Generate the garland
spiral_points = list()
for(i in 1:330) {
  spiral_points[[i]] = matrix(c(sqrt(i)/10/1.75*sinpi(-i/30),1.4-i/170, sqrt(i)/10/1.75*cospi(-i/30)),ncol=3)
}
spiral_line = path(spiral_points,material=glossy(color="gold"), width = 0.05, type="flat")

#Spin the tree
tree_angle = 30*sinpi(1:120*3/180)

#Add some camera shake
camera_pos_x = ambient::gen_perlin(x=2*sinpi(1:120*3/180), z=2*cospi(1:120*3/180), frequency = 1/6, seed = 7)/10
camera_pos_y = ambient::gen_perlin(x=2*sinpi(1:120*3/180), z=2*cospi(1:120*3/180), frequency = 1/6, seed = 8)/10
camera_pos_z = ambient::gen_perlin(x=2*sinpi(1:120*3/180), z=2*cospi(1:120*3/180), frequency = 1/6, seed = 17)/2

#Render each scene (tryCatch is to keep the loop going when it runs into a singular matrix (bug)
#—need to rerun those iterations at the end
for(i in 1:120) {
  tryCatch({
    generate_ground(material = diffuse(color="darkred"),spheresize = 10) %>% 
      #Tree body
      add_object(csg_object(branches0, y=-0.5,material=glossy(color="green"))) %>% 
      add_object(csg_object(branches1, material=glossy(color="green"))) %>% 
      add_object(csg_object(branches2,y=0.5, material=glossy(color="green"))) %>% 
      add_object(csg_object(branches3,y=0.95, material=glossy(color="green"))) %>% 
      add_object(segment(start=c(0,-1,0),end=c(0,0,0),radius=0.2, material=glossy(color="brown"))) %>% 
      #Ornaments
      add_object(bulbs_full) %>% 
      add_object(spiral_line) %>% 
      add_object(lights_full) %>% 
      add_object(obj_model(r_obj(), x=0,y=1.4,scale_obj = 0.25,
                           material = microfacet(roughness=0.3,
                                                 eta=c(0.216,0.42833,1.3184), kappa=c(3.239,2.4599,1.8661)))) %>% 
      #Tree rotation
      group_objects(angle=c(0,tree_angle[i],0)) %>% 
      #Presents
      add_object(cube(x=0.5,y=-0.8,z=0.4,width = 0.4, 
                      material = glossy(checkercolor = "red",color="darkgreen", checkerperiod = 0.1))) %>% 
      add_object(cube(x=-0.5,y=-0.8,z=0.4,width = 0.4, 
                      material = glossy(checkercolor = "gold",color="blue", checkerperiod = 0.1),angle=c(0,30,0))) %>% 
      add_object(cube(x=0,y=-0.9,z=1,ywidth=0.2,xwidth=0.8,zwidth=0.4,
                      material = glossy(color = "red"))) %>% 
      add_object(cube(x=0,y=-0.9,z=1,ywidth=0.205,xwidth=0.805,zwidth=0.05,
                      material = glossy(color = "white"))) %>% 
      
      add_object(cube(x=0,y=-0.9,z=1,ywidth=0.205,xwidth=0.055,zwidth=0.405,
                      material = glossy(color = "white"))) %>% 
      add_object(disk(y=-0.98,radius=1.5,material=diffuse(color="darkgreen"))) %>% 
      add_object(disk(y=-0.99,radius=1.6,material=diffuse(color="white"))) %>% 
      #Fill lighting
      add_object(sphere(y=5,z=3,material=light(intensity = 10))) %>%
      render_scene(samples=256,  verbose = T,
                   filename=sprintf("christmas_tree%d",i),
                   environment_light = "winter_evening_2k.hdr",
                   camera_description_file = "50mm",
                   aperture=50, width=800,height=800, clamp_value=10,
                   fov = 12,sample_method = "sobol_blue", rotate_env = -105, film_size = 44,
                   iso=200,camera_scale = 10,
                   lookfrom = c(camera_pos_x[i],2+camera_pos_y[i],7+camera_pos_z[i]),
                   lookat=c(0,0.1,0),focal_distance = 6.5)
  }, error = function(e){
    print(sprintf("Need to re-run #%d",i))})
}



# TEST?
generate_ground(material = diffuse(color="darkred"),spheresize = 10) %>% 
  #Tree body
  add_object(csg_object(branches0, y=-0.5,material=glossy(color="green"))) %>% 
  add_object(csg_object(branches1, material=glossy(color="green"))) %>% 
  add_object(csg_object(branches2,y=0.5, material=glossy(color="green"))) %>% 
  add_object(csg_object(branches3,y=0.95, material=glossy(color="green"))) %>% 
  add_object(segment(start=c(0,-1,0),end=c(0,0,0),radius=0.2, material=glossy(color="brown"))) %>% 
  #Ornaments
  add_object(bulbs_full) %>% 
  add_object(spiral_line) %>% 
  add_object(lights_full) %>% 
  add_object(obj_model(r_obj(), x=0,y=1.4,scale_obj = 0.25,
                       material = microfacet(roughness=0.3,
                                             eta=c(0.216,0.42833,1.3184), kappa=c(3.239,2.4599,1.8661)))) %>% 
  #Tree rotation
  group_objects(angle=c(0,tree_angle[1],0)) %>% 
  #Presents
  add_object(cube(x=0.5,y=-0.8,z=0.4,width = 0.4, 
                  material = glossy(checkercolor = "red",color="darkgreen", checkerperiod = 0.1))) %>% 
  add_object(cube(x=-0.5,y=-0.8,z=0.4,width = 0.4, 
                  material = glossy(checkercolor = "gold",color="blue", checkerperiod = 0.1),angle=c(0,30,0))) %>% 
  add_object(cube(x=0,y=-0.9,z=1,ywidth=0.2,xwidth=0.8,zwidth=0.4,
                  material = glossy(color = "red"))) %>% 
  add_object(cube(x=0,y=-0.9,z=1,ywidth=0.205,xwidth=0.805,zwidth=0.05,
                  material = glossy(color = "white"))) %>% 
  
  add_object(cube(x=0,y=-0.9,z=1,ywidth=0.205,xwidth=0.055,zwidth=0.405,
                  material = glossy(color = "white"))) %>% 
  add_object(disk(y=-0.98,radius=1.5,material=diffuse(color="darkgreen"))) %>% 
  add_object(disk(y=-0.99,radius=1.6,material=diffuse(color="white"))) %>% 
  #Fill lighting
  add_object(sphere(y=5,z=3,material=light(intensity = 10))) %>%
  render_scene(samples=256,  verbose = T,
               filename=sprintf("christmas_tree%d",i),
               environment_light = "winter_evening_2k.hdr",
               camera_description_file = "50mm",
               aperture=50, width=800,height=800, clamp_value=10,
               fov = 12,sample_method = "sobol_blue", rotate_env = -105, film_size = 44,
               iso=200,camera_scale = 10,
               lookfrom = c(camera_pos_x[1],2+camera_pos_y[1],7+camera_pos_z[1]),
               lookat=c(0,0.1,0),focal_distance = 6.5)
