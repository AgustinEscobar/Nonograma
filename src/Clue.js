import React from 'react';

class Clue extends React.Component {
    render() {
        
        const clue = this.props.clue;

       /*
        const {satisfaction,index,clue} = this.props;
        */
        return (
            <div className={this.props.satisfaction === 1 ? "clue sat" : "clue"} >
                {clue.map((num, i) =>
                    <div key={i}>
                        {num}
                    </div>
                )}
            </div>    
        );
    }
}

export default Clue;