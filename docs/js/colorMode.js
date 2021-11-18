
const bodyElement = document.getElementById('page-body');

function replaceBodyElementClass(className, newClassName) {
    const newFullClass = bodyElement.className.replace(className, newClassName);
    bodyElement.className = newFullClass;
}

const LIGHT_MODE = 'light-mode';
const DARK_MODE = 'dark-mode';

document.getElementById(LIGHT_MODE)
    .addEventListener('click', () => replaceBodyElementClass(DARK_MODE, LIGHT_MODE));

document.getElementById(DARK_MODE)
    .addEventListener('click', () => replaceBodyElementClass(LIGHT_MODE, DARK_MODE));
