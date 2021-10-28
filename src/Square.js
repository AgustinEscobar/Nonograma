import React from 'react';

class Square extends React.Component {
    render() {
        return (
            <button className={this.props.value === "#" ? "square pintado" : "square"} onClick={this.props.onClick}>
                {this.props.value !== '_' ? this.props.value : null}
            </button>
        );
    }
}

export default Square;