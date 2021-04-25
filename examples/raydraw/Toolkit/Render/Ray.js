
exports.createVec3 = function (x) {
    return function (y) {
        return function (z) {
            return new THREE.Vector3(x, y, z);
        };
    };
};

exports.renderNativeRay = function (points) {
 return function (color1) {
    return function (color2) {
        return function (color3) {
            return function (fragmentShader) {
                return function (vertexShader) {
                    return function () {

                        var mesh, scene, camera, renderer;

                        var t = 0;

                        init();
                        animate();

                        function init() {
                            //Renderer

                            var maybeCanvas = document.querySelector('#previewCanvas');
                            if (maybeCanvas != null) {
                                maybeCanvas.remove();
                            }
    
                            var canvas;
                            canvas = document.createElement("canvas");
                            canvas.id = "previewCanvas";
    
                            canvas.width = window.innerWidth;
                            canvas.height = window.innerHeight;
    
                            const network = document.querySelector('#network');
                            network.appendChild(canvas);
    
                            renderer = new THREE.WebGLRenderer({
                                canvas: canvas,
                                antialias: true
                            });
                            renderer.setClearColor(0xccffff);
                            renderer.setPixelRatio(window.devicePixelRatio);
                            renderer.setSize(window.innerWidth, window.innerHeight);

                            //Camera
                            camera = new THREE.PerspectiveCamera(
                                35,
                                window.innerWidth / window.innerHeight,
                                1,
                                3000
                            );
                            camera.position.y = -1;

                            //Scene
                            scene = new THREE.Scene();

                            function addLight(...pos) {
                                const color = 0xffffff;
                                const intensity = 1;
                                const light = new THREE.DirectionalLight(color, intensity);
                                light.position.set(...pos);
                                scene.add(light);
                            }
                            addLight(-1, 2, 4);
                            addLight(-3, -2, -3);

                            const vertices = [];
                            points.forEach(pair => {
                                vertices.push({pos : [pair[0], -1, pair[1]]});
                                vertices.push({pos : [pair[0], 1, pair[1]]});
                            });
                             
                            const numVertices = vertices.length;
                            const positionNumComponents = 3;
                            const positions = new Float32Array(numVertices * positionNumComponents);
                            let posNdx = 0;
                            for (const vertex of vertices) {
                                positions.set(vertex.pos, posNdx);
                                posNdx += positionNumComponents;
                            }

                            const geometry = new THREE.BufferGeometry();
                            geometry.setAttribute(
                                "position",
                                new THREE.BufferAttribute(positions, positionNumComponents)
                            );

                            var geometryIndicies = [];

                            //     1  3  5  7    3 + i*2
                            //     o--o--o--o-..-o
                            //     | /| /| /|   /|
                            //     |/ |/ |/ |/   |
                            //     o--o--o--o-..-o
                            //     0  2  4  6    2 + i*2

                            for (var i = 0; i < points.length - 1; i++) {
                                // upper vertex
                                geometryIndicies.push(0 + i*2);
                                geometryIndicies.push(3 + i*2);
                                geometryIndicies.push(1 + i*2);

                                // lower vertex
                                geometryIndicies.push(3 + i*2);
                                geometryIndicies.push(0 + i*2);
                                geometryIndicies.push(2 + i*2);
                            }                            
                            geometry.setIndex(geometryIndicies);
                            geometry.computeVertexNormals();

                            var numVertices2 = geometry.attributes.position.count;
                            var distance = new Float32Array(numVertices2 * 1); // 1 value per vertex
                            geometry.addAttribute('distance', new THREE.BufferAttribute(distance, 1));

                            // populate attribute
                            for (var i = 0; i < numVertices2; i++) {
                                var numVertex = Math.floor(i / 2); //going through verticies along line     

                                // making ray thinner at the beginning
                                var yy = geometry.attributes.position.getY(i);
                                var k = (0.7 + 0.3 * (numVertex / (numVertices / 2 - 1)));
                                geometry.attributes.position.setY(i, yy * k);

                                distance[i] = numVertex / (numVertices / 2 - 1);
                            }


                            // uniforms
                            var uniforms = {
                                "fraction": { value: 0 },
                                colorA: new THREE.Uniform(color1),
                                colorB: new THREE.Uniform(color2),
                                colorC: new THREE.Uniform(color3),
                            };

                            // material
                            var material = new THREE.ShaderMaterial({
                                uniforms: uniforms,
                                vertexShader: vertexShader,
                                fragmentShader: fragmentShader,
                                side: THREE.DoubleSide
                            });
                            mesh = new THREE.Mesh(geometry, material);

                            scene.add(mesh);
                            mesh.position.set(0, 0, -20);

                            mesh.rotation.x = 0.4;
                        }

                        function animate() {

                            requestAnimationFrame(animate);
                            t += 0.010;

                            mesh.material.uniforms.fraction.value = 0.5 * (1 + Math.sin(t - Math.PI / 2));
                            render();
                        }

                        function render() {
                            renderer.render(scene, camera);
                        }

                    };
                };
            };
        };
    };
};
};