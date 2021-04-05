
exports.renderNativeRay = function (fragmentShader) {
    return function (vertexShader) {
        return function () {


            var maybeCanvas = document.querySelector('#previewCanvas');
            if (maybeCanvas != null) {
                maybeCanvas.remove();
            }   

            var canvas;            
            canvas = document.createElement("canvas");
            canvas.id = "previewCanvas";
            
            canvas.width  = window.innerWidth;
            canvas.height = window.innerHeight;

            const network = document.querySelector('#network');
            network.appendChild(canvas);

            const renderer = new THREE.WebGLRenderer({ canvas });

            var wWidth = window.innerWidth;
            var wHeight = window.innerHeight;
            
            const fov = 75;
            const aspect = 300 / 150;  // the canvas default (because 300x150)
            const near = 0.1;
            const far = 5;
            const camera = new THREE.PerspectiveCamera(60, window.innerWidth / window.innerHeight, 0.01, 100);
            camera.position.z = wWidth < 800 ? 80 : 60;
            camera.position.x = wWidth < 800 ? 0 : 5;

            const scene = new THREE.Scene();

            {
                const color = 0xFFFFFF;
                const intensity = 1;
                const light = new THREE.DirectionalLight(color, intensity);
                light.position.set(-1, 2, 4);
                scene.add(light);
            }

            // const boxWidth = 1;
            // const boxHeight = 1;
            // const boxDepth = 1;
            // const geometry = new THREE.BoxGeometry(boxWidth, boxHeight, boxDepth);

            //const material = new THREE.MeshPhongMaterial({ color: 0x44aa88 });  // greenish blue

            const material = new THREE.ShaderMaterial({
                uniforms: {
                    posX: { type: 'f', value: 0 },
                    posY: { type: 'f', value: 0 },
                },
                vertexShader: vertexShader,
                fragmentShader: fragmentShader,
                side: THREE.DoubleSide,
            });

            function makeInstance(geometry, x) {

                const mesh = new THREE.Mesh(geometry, material);
                scene.add(mesh);

                mesh.position.x = x;

                return mesh;
            }

            const geometry = new THREE.PlaneGeometry(50, 50, 6);
            const objs = [
                makeInstance(geometry, 0)
            ];

            function render(time) {
                time *= 0.001;  // convert time to seconds

                objs.forEach((mesh, ndx) => {
                    const speed = 1;
                    const rot = time * speed;
                    // mesh.rotation.x = rot;
                    mesh.rotation.y = rot;
                });

                renderer.render(scene, camera);

                requestAnimationFrame(render);
            }
            requestAnimationFrame(render);
        };
    };
};